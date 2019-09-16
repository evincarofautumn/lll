{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LLL where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Foldable (for_, toList)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Data.Sequence ((|>))
import Data.Text (Text)
import Data.Traversable (for)
import GHC.Generics
import System.IO
import Text.PrettyPrint ((<+>), hcat, hsep, nest, punctuate, sep, vcat)
import Text.PrettyPrint (Doc)
import Text.PrettyPrint.HughesPJ (maybeParens)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Text as Text
import qualified Text.PrettyPrint as Pretty

--------------------------------------------------------------------------------
-- Programs and Terms
--------------------------------------------------------------------------------

data Program = Program { programStatements :: [Statement] }

data Statement
  = Define !Bind !Expr

data Expr where

  -- | > <and-intro> ::= <expr> ',' <expr>
  AndIntro :: !Expr -> !Expr -> Expr

  -- | > <and-elim> ::= 'let' <bind> ',' <bind> '=' <expr> 'in' <expr>
  AndElim :: !Bind -> !Bind -> !Expr -> !Expr -> Expr

  -- | > <or-intro> ::= 'left' <expr> | 'right' <expr>
  OrIntro :: !OrTag -> !Expr -> Expr

  -- |
  -- > <or-elim> ::= 'match' <expr> 'with'
  -- >   '|' 'left' <bind> '->' <expr>
  -- >   '|' 'right' <bind> '->' <expr>
  OrElim :: !Expr -> !Bind -> !Expr -> !Bind -> !Expr -> Expr

  -- | > <with-intro> ::= <expr> '&' <expr>
  WithIntro :: !Expr -> !Expr -> Expr

  -- | > <with-elim> ::= 'first' <expr> | 'second' <expr>
  WithElim :: !WithTag -> !Expr -> Expr

  -- | > <par-intro> ::= <expr> '|' <expr>
  ParIntro :: !Expr -> !Expr -> Expr

  -- |
  -- > <par-elim> ::= 'fork' <expr> 'into'
  -- >   '|' <bind> '->' <expr>
  -- >   '|' <bind> '->' <expr>
  ParElim :: !Expr -> !Bind -> !Expr -> !Bind -> !Expr -> Expr

  -- | > <unit-intro> ::= '()'
  UnitIntro :: Expr

  -- | > <bool-intro> ::= 'true' | 'false'
  BoolIntro :: !Bool -> Expr

  -- | > <bool-elim> ::= 'if' <expr> 'then' <expr> 'else' <expr>
  BoolElim :: !Expr -> !Expr -> !Expr -> Expr

  -- | > <number-intro> ::= <number>
  NumberIntro :: !Number -> Expr

  -- | > <unary-elim> ::= '-' <expr> | 'abs' <expr> | ...
  UnaryElim :: !Unary -> !Expr -> Expr

  -- | > <binary-elim> ::= <expr> '+' <expr> | <expr> '-' <expr> | ...
  BinaryElim :: !Binary -> !Expr -> !Expr -> Expr

  -- | > <let> ::= 'let' <bind> '=' <expr> 'in' <expr>
  Let :: !Bind -> !Expr -> !Expr -> Expr
  
  -- > <implies-intro> ::= '\' <bind> '->' <expr>
  ImpliesIntro :: !Bind -> !Expr -> Expr

  -- > <implies-elim> ::= <expr> <expr>
  ImpliesElim :: !Expr -> !Expr -> Expr

  -- | > <group> ::= '(' <expr> ')' | 'begin' <expr> 'end'
  Group :: !GroupStyle -> !Expr -> Expr

  -- | > <var> ::= <name>
  Var :: !Name -> Expr

  -- | > <as> ::= <expr> ':' <type>
  As :: !Expr -> !Type -> Expr

  -- | > <abort> ::= 'abort'
  Abort :: Expr

  -- | > <sequence> ::= <expr> ';' <expr>
  Sequence :: !Expr -> !Expr -> Expr

  -- | > <trace> ::= 'trace' <expr>
  Trace :: !Expr -> Expr

  -- | > <bottom-intro> ::= 'done'
  --
  -- UNSOUND, for debugging purposes.
  BottomIntro :: Expr

  deriving stock (Eq, Show)

data OrTag
  = LeftTag
  | RightTag
  deriving stock (Eq, Show)

data WithTag
  = FirstTag
  | SecondTag
  deriving stock (Eq, Show)

data GroupStyle
  = Parentheses
  | BeginEnd
  deriving stock (Eq, Show)

data Unary
  = Negation
  | AbsoluteValue
  | LogicalNot
  deriving stock (Eq, Show)

namedUnary :: Unary -> Bool
namedUnary = \ case
  Negation -> False
  AbsoluteValue -> True
  LogicalNot -> True

data Binary
  = Addition
  | Subtraction
  | Multiplication
  | Division
  | Modulus
  | LogicalAnd
  | LogicalOr
  | Equality
  | LessThan
  | GreaterThan
  | LessThanOrEqual
  | GreaterThanOrEqual
  | NotEqual
  deriving stock (Eq, Show)

binaryPrec :: Binary -> Rational
binaryPrec = \ case
  Addition -> 6
  Subtraction -> 6
  Multiplication -> 7
  Division -> 7
  Modulus -> 7
  LogicalAnd -> 3
  LogicalOr -> 2
  Equality -> 4
  LessThan -> 4
  GreaterThan -> 4
  LessThanOrEqual -> 4
  GreaterThanOrEqual -> 4
  NotEqual -> 4

-- | > <bind> ::= <name> (':' <type>)?
data Bind = Bind !Name !(Maybe Type)
  deriving stock (Eq, Show)

-- | > <number> ::= /[+-]?[0-9]+/
type Number = Integer

-- | > <name> ::= /[A-Za-z_][0-9A-Za-z_]*/
newtype Name = Name { nameText :: Text }
  deriving stock (Show)
  deriving newtype (Eq)

data Type where

  -- | > <and> ::= <type> '*' <type>
  And :: !Type -> !Type -> Type

  -- | > <or> ::= <type> '+' <type>
  Or :: !Type -> !Type -> Type

  -- | > <with> ::= <type> '&' <type>
  With :: !Type -> !Type -> Type

  -- | > <par> ::= <type> '|' <type>
  Par :: !Type -> !Type -> Type

  -- | > <implies> ::= <type> '->' <type>
  Implies :: !Type -> !Type -> Type

  -- | > <number> ::= 'int'
  Number :: Type

  -- | > <unit> ::= '()'
  Unit :: Type

  -- | > <bottom> ::= 'done'
  Bottom :: Type

  -- | > <top> ::= 'top'
  Top :: Type

  -- | > <void> ::= 'impossible'
  Void :: Type

  -- | > <bool> ::= 'bool'
  Bool :: Type

  -- | > <of-course> ::= '!' <type>
  OfCourse :: !Type -> Type

  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Pretty Binary where
  pPrint = \ case
    Addition -> "+"
    Subtraction -> "-"
    Multiplication -> "*"
    Division -> "/"
    Modulus -> "%"
    LogicalAnd -> "&&"
    LogicalOr -> "||"
    Equality -> "="
    LessThan -> "<"
    GreaterThan -> ">"
    LessThanOrEqual -> "<="
    GreaterThanOrEqual -> ">="
    NotEqual -> "<>"

instance Pretty Bind where
  pPrintPrec l p (Bind name maybeType) = hsep
    $ pPrint name
    : concat
      [ [typeAscriptionOperator, pPrintPrec l p type_]
      | Just type_ <- pure maybeType
      ]

instance Pretty Expr where

  pPrintPrec l = go
    where

      go p = \ case

        AndIntro e1 e2 -> maybeParens (p >= andExprPrec)
          $ sep $ punctuate andOperator $ go andExprPrec <$> [e1, e2]

        AndElim x y e1 e2 -> maybeParens (p >= appExprPrec)
          $ vcat
            [ letKeyword <+> sep
              [ hsep
                [ sep $ punctuate andOperator $ pPrint <$> [x, y]
                , variableBindingSeparator
                ]
              , hsep [go exprPrec e1, letBodySeparator]
              ]
            , go exprPrec e2
            ]

        OrIntro d e -> maybeParens (p >= appExprPrec)
          $ sep
            [ case d of
              LeftTag -> orLeftKeyword
              RightTag -> orRightKeyword
            , go appExprPrec e
            ]

        OrElim e1 x e2 y e3 -> maybeParens (p >= appExprPrec)
          $ sep
            [ hsep [orElimKeyword, go exprPrec e1, orElimBodySeparator]
            , nest indent
              $ sep
                [ hsep
                  [ branchPrefix
                  , orLeftKeyword
                  , pPrint x
                  , branchBodySeparator
                  ]
                , nest indent $ go exprPrec e2
                ]
            , nest indent
              $ sep
                [ hsep
                  [ branchPrefix
                  , orRightKeyword
                  , pPrint y
                  , branchBodySeparator
                  ]
                , nest indent $ go exprPrec e3
                ]
            ]

        WithIntro e1 e2 -> maybeParens (p >= withExprPrec)
          $ sep $ punctuate' (withOperator <> " ") $ go withExprPrec <$> [e1, e2]

        WithElim d e -> maybeParens (p >= appExprPrec)
          $ sep
            [ case d of
              FirstTag -> withFirstKeyword
              SecondTag -> withSecondKeyword
            , go appExprPrec e
            ]

        ParIntro e1 e2 -> maybeParens (p >= parExprPrec)
          $ sep $ punctuate' (parIntroOperator <> " ")
          $ go parExprPrec <$> [e1, e2]

        ParElim e1 x e2 y e3 -> maybeParens (p >= appExprPrec) $ sep
          [ hsep [parElimKeyword, go exprPrec e1, parElimBodySeparator]
          , nest indent
            $ sep
              [ hsep [branchPrefix, pPrint x, branchBodySeparator]
              , nest indent $ go exprPrec e2
              ]
          , nest indent
            $ sep
              [ hsep [branchPrefix, pPrint y, branchBodySeparator]

              , nest indent $ go exprPrec e3
              ]
          ]

        UnitIntro -> "()"

        BoolIntro d -> if d then trueKeyword else falseKeyword

        BoolElim e1 e2 e3 -> maybeParens (p >= appExprPrec) $ sep
          [ hsep [ifKeyword, go exprPrec e1]
          , nest indent
            $ hsep [ifBodySeparator, go exprPrec e2]
          , nest indent
            $ hsep [elseKeyword, go exprPrec e3]
          ]

        NumberIntro n -> pPrint n

        UnaryElim op e
          -> (if namedUnary op then sep else hcat)
            [ pPrint op
            , go appExprPrec e
            ]

        BinaryElim op e1 e2 -> let
          p' = binaryPrec op
          in maybeParens (p >= p')
            $ sep
              [ go p' e1
              , hsep [pPrint op, go p' e2]
              ]

        Let x e1 e2 -> maybeParens (p >= appExprPrec) $ vcat
          [ letKeyword <+> sep
            [ hsep [pPrint x, variableBindingSeparator]
            , hsep [go exprPrec e1, letBodySeparator]
            ]
          , go exprPrec e2
          ]

        ImpliesIntro x e -> maybeParens (p >= appExprPrec)
          $ sep
            [ hsep [lambda, pPrintPrec l impliesTypePrec x, lambdaBodySeparator]
            , go exprPrec e
            ]

        ImpliesElim e1 e2 -> maybeParens (p >= appExprPrec)
          $ sep $ go appExprPrec <$> [e1, e2]

        Group d e -> case d of
          Parentheses -> Pretty.parens (go exprPrec e)
          BeginEnd -> vcat [begin, nest indent $ go exprPrec e, end]

        Var name -> pPrint name

        As e t -> maybeParens (p >= asExprPrec)
          $ sep
            [ go asExprPrec e
            , hsep [typeAscriptionOperator, pPrint t]
            ]

        Abort -> abortKeyword

        Sequence e1 e2 -> maybeParens (p >= seqExprPrec)
          $ vcat
            [ hcat [go seqExprPrec e1, sequenceOperator]
            , go seqExprPrec e2
            ]

        Trace e -> sep [traceKeyword, go appExprPrec e]

        BottomIntro -> bottomKeyword

      indent = 2

exprPrec = 0
seqExprPrec = 1
asExprPrec = 2
-- orExprPrec = 3
withExprPrec = 4
parExprPrec = 5
andExprPrec = 6
appExprPrec = 10

instance Pretty Name where
  pPrint = Pretty.text . Text.unpack . nameText

instance Pretty Program where
  pPrint = vcat . intersperse "" . fmap pPrint . programStatements

instance Pretty Statement where
  pPrint (Define x e) = definitionKeyword <+> sep
    [ hsep [pPrint x, definitionBindingSeparator]
    , pPrint e
    ]

instance Pretty Type where

  pPrintPrec _l = go
    where
      go p = \ case

        And t1 t2 -> maybeParens (p >= andTypePrec)
          $ sep [go andTypePrec t1, hsep [andTypeOperator, go andTypePrec t2]]

        Or t1 t2 -> maybeParens (p >= orTypePrec)
          $ sep [go orTypePrec t1, hsep [orTypeOperator, go orTypePrec t2]]

        With t1 t2 -> maybeParens (p >= withTypePrec)
          $ sep
            [ go withTypePrec t1
            , hsep [withTypeOperator, go withTypePrec t2]
            ]

        Par t1 t2 -> maybeParens (p >= parTypePrec)
          $ sep [go parTypePrec t1, hsep [parTypeOperator, go parTypePrec t2]]

        Implies t1 t2 -> maybeParens (p >= impliesTypePrec)
          $ sep
            [ go impliesTypePrec t1
            , hsep [impliesTypeOperator, go impliesTypePrec t2]
            ]

        Number -> "integer"
        Unit -> "()"
        Bottom -> bottomKeyword
        Top -> "top"
        Void -> "impossible"
        Bool -> "bool"
        OfCourse t -> sep ["source", go appTypePrec t]

impliesTypePrec = 1
orTypePrec = 3
withTypePrec = 4
parTypePrec = 5
andTypePrec = 6
appTypePrec = 10

instance Pretty Unary where
  pPrint = \ case
    Negation -> "-"
    AbsoluteValue -> "abs"
    LogicalNot -> "not"

abortKeyword               = "abort"
andOperator                = ","
andTypeOperator            = "*"
arrow                      = "->"
begin                      = "begin"
bottomKeyword              = "done"
branchBodySeparator        = arrow
branchPrefix               = "|"
definitionBindingSeparator = "="
definitionKeyword          = "def"
elseKeyword                = "else"
end                        = "end"
falseKeyword               = "false"
ifBodySeparator            = "then"
ifKeyword                  = "if"
impliesTypeOperator        = arrow
lambda                     = "fun"
lambdaBodySeparator        = "->"
letBodySeparator           = "in"
letKeyword                 = "let"
orElimBodySeparator        = "with"
orElimKeyword              = "match"
orLeftKeyword              = "left"
orRightKeyword             = "right"
orTypeOperator             = "+"
parElimBodySeparator       = "into"
parElimKeyword             = "fork"
parIntroOperator           = "|"
parTypeOperator            = "|"
sequenceOperator           = ";"
traceKeyword               = "trace"
trueKeyword                = "true"
typeAscriptionOperator     = ":"
variableBindingSeparator   = "="
withFirstKeyword           = "fst"
withOperator               = "&"
withSecondKeyword          = "snd"
withTypeOperator           = "&"

applyIf :: Bool -> (a -> a) -> a -> a
applyIf b f x = if b then f x else x

-- | Like 'punctuate', but prefixing the separator rather than suffixing it, for
-- wrapping purposes.
punctuate' :: Doc -> [Doc] -> [Doc]
punctuate' s ds = case ds of
  [] -> ds
  [_] -> ds
  d : ds' -> d : fmap (s <>) ds'

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

data Value
  = AndValue !Value !Value
  | WithValue !Env !Expr !Expr
  | OrValue !OrTag !Value
  | ParValue !Env !Expr !Expr
  | UnitValue
  | BoolValue !Bool
  | NumberValue !Number
  | ImpliesValue !Env !Bind !Expr
  deriving stock (Eq, Show)

data Env = Env
  { envBinds :: [(Name, Value)]
  , envOutputChannel :: !(Maybe (Chan (Maybe Value)))
  } deriving (Eq)

instance Show Env where
  show env = concat
    [ "Env { envBinds = "
    , show $ envBinds env
    , ", envOutputChannel = "
    , case envOutputChannel env of
      Nothing -> "Nothing"
      Just _chan -> "Just (error \"cannot show channel\")"
    , " }"
    ]

newtype Eval a = Eval { unEval :: ReaderT Env (MaybeT IO) a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

done :: Eval a
done = Eval $ ReaderT $ \ _env -> MaybeT $ pure Nothing

-- | Evaluate an expression; close its output channel.
evaluate :: Expr -> IO (Maybe Value)
evaluate expr = flip runEval initialEnv $ eval expr <* closeOutputChannel

-- | Evaluate an expression with the given output channel and close it.
evaluateChan :: Chan (Maybe Value) -> Expr -> IO (Maybe Value)
evaluateChan chan expr
  = flip runEval initialEnv { envOutputChannel = Just chan }
  $ eval expr <* closeOutputChannel

closeOutputChannel :: Eval ()
closeOutputChannel = do
  maybeChan <- getsEnv envOutputChannel
  case maybeChan of
    Just chan -> liftIO $ writeChan chan Nothing
    Nothing -> pure ()

collectOutput :: Chan (Maybe Value) -> IO [Value]
collectOutput chan = loop mempty
  where
    loop acc = readChan chan >>= \ case
      Nothing -> pure (toList acc)
      Just value -> loop (acc |> value)

runEval :: Eval a -> Env -> IO (Maybe a)
runEval e env = runMaybeT $ flip runReaderT env $ unEval e

{-
instance MonadFail Eval where
  fail = raise . InternalError . Text.pack
-}

eval :: Expr -> Eval Value
eval = \ case

  AndIntro e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    pure $ AndValue v1 v2

  AndElim (Bind x _t1) (Bind y _t2) e1 e2 -> do
    (v1, v2) <- assertAnd =<< eval e1
    binding x v1 $ binding y v2 $ eval e2

  OrIntro d e -> do
    v <- eval e
    pure $ OrValue d v

  OrElim e1 (Bind x _t1) e2 (Bind y _t2) e3 -> do
    (d, v) <- assertOr =<< eval e1
    case d of
      LeftTag -> binding x v (eval e2)
      RightTag -> binding y v (eval e3)

  WithIntro e1 e2 -> do
    staticEnv <- getEnv
    pure $ WithValue staticEnv e1 e2

  WithElim d e -> do
    (env, a, b) <- assertWith =<< eval e
    locally (const env) $ eval case d of
      FirstTag -> a
      SecondTag -> b

  ParIntro e1 e2 -> do
    staticEnv <- getEnv
    pure $ ParValue staticEnv e1 e2

  ParElim e1 (Bind x _t1) e2 (Bind y _t2) e3 -> do

    (parEnv, a, b) <- assertPar =<< eval e1

    result <- liftIO do

      (taskA, taskB) <- (,)
        <$> (async . flip runEval parEnv) do
          v1 <- eval a
          binding x v1 (eval e2)
        <*> (async . flip runEval parEnv) do
          v2 <- eval b
          binding y v2 (eval e3)

      -- Wait for both tasks, catching exceptions from either.
      -- try (waitBoth taskA taskB)
      oneOfMany [taskA, taskB]

    case result of

      -- If any task aborted, abort.
      Left ex -> raise (ex :: SomeException)

      -- If all tasks are done, done.
      Right Nothing -> done

      -- If one task returned, return.
      Right (Just res) -> pure res

{-
      -- If both tasks send, complete.
      Right (Nothing, Nothing) -> done

      -- If A returns C and B sends, return C.
      Right (Just c, Nothing) -> pure c

      -- If A sends and B returns C, return C.
      Right (Nothing, Just c) -> pure c

      -- If both tasks return, this is an internal error.
      Right (Just c, Just c') -> raise $ InternalError $ mconcat
        [ "parallel tasks both returned: "
        , Text.pack $ show c
        , " and "
        , Text.pack $ show c'
        ]
-}

  UnitIntro -> do
    pure UnitValue

  BoolIntro d -> do
    pure $ BoolValue d

  BoolElim e1 e2 e3 -> do
    v1 <- assertBool =<< eval e1
    eval if v1 then e2 else e3

  NumberIntro n -> do
    pure $ NumberValue n

  UnaryElim op e1 -> do
    v1 <- eval e1
    case (v1, op) of
      (NumberValue n, Negation) -> pure $ NumberValue $ -n
      (NumberValue n, AbsoluteValue) -> pure $ NumberValue $ abs n
      (BoolValue d, LogicalNot) -> pure $ BoolValue $ not d
      _ -> abort "type error"

  BinaryElim op e1 e2 -> case op of

    Addition -> do
      v1 <- assertNumber =<< eval e1
      v2 <- assertNumber =<< eval e2
      pure $ NumberValue $ v1 + v2

    Subtraction -> do
      v1 <- assertNumber =<< eval e1
      v2 <- assertNumber =<< eval e2
      pure $ NumberValue $ v1 - v2

    Multiplication -> do
      v1 <- assertNumber =<< eval e1
      v2 <- assertNumber =<< eval e2
      pure $ NumberValue $ v1 * v2

    Division -> do
      v1 <- assertNumber =<< eval e1
      v2 <- assertNumber =<< eval e2
      if v2 == 0
        then abort "division by zero"
        else pure $ NumberValue $ v1 `div` v2

    Modulus -> do
      v1 <- assertNumber =<< eval e1
      v2 <- assertNumber =<< eval e2
      if v2 == 0
        then abort "modulus by zero"
        else pure $ NumberValue $ v1 `mod` v2

    LogicalAnd -> do
      d <- assertBool =<< eval e1
      if d then eval e2 else pure $ BoolValue False

    LogicalOr -> do
      d <- assertBool =<< eval e1
      if d then pure $ BoolValue True else eval e2

    Equality -> relational e1 e2 (==)
    LessThan -> relational e1 e2 (<)
    GreaterThan -> relational e1 e2 (>)
    LessThanOrEqual -> relational e1 e2 (<=)
    GreaterThanOrEqual -> relational e1 e2 (>=)
    NotEqual -> relational e1 e2 (/=)

  Let (Bind x _t) e1 e2 -> do
    v1 <- eval e1
    binding x v1 (eval e2)

  ImpliesIntro x e -> do
    staticEnv <- getEnv
    pure $ ImpliesValue staticEnv x e

  ImpliesElim e1 e2 -> do
    (staticEnv, Bind x _t, e) <- assertImplies =<< eval e1
    v2 <- eval e2
    locally (const staticEnv) $ binding x v2 $ eval e

  Group _ e -> eval e

  Var x -> envLookup x

  As e _t -> eval e

  Abort -> abort "abort"

  Sequence e1 e2 -> eval e1 *> eval e2

  -- Write a value to the output channel, if one is specified.
  Trace e -> do
    v <- eval e
    maybeChan <- getsEnv envOutputChannel
    case maybeChan of
      Just chan -> liftIO $ writeChan chan $ Just v
      Nothing -> pure ()
    pure UnitValue

  BottomIntro -> do
    done

  where

    eagerBinary :: Expr -> Expr -> (Value -> Value -> Eval a) -> Eval a
    eagerBinary e1 e2 f = do
      v1 <- eval e1
      v2 <- eval e2
      f v1 v2

    relational
      :: Expr
      -> Expr
      -> (forall a. Ord a => a -> a -> Bool)
      -> Eval Value
    relational e1 e2 f = eagerBinary e1 e2 $ curry \ case
      (NumberValue v1, NumberValue v2) -> pure $ BoolValue $ f v1 v2
      (BoolValue v1, BoolValue v2) -> pure $ BoolValue $ f v1 v2
      (v1, v2) -> abort $ mconcat
        [ "type mismatch between "
        , Text.pack $ show v1
        , " and "
        , Text.pack $ show v2
        ]

assertNumber :: Value -> Eval Number
assertNumber = \ case
  NumberValue v -> pure v
  other -> abort $ mconcat
    [ "expected number but got "
    , Text.pack $ show other
    ]

assertBool :: Value -> Eval Bool
assertBool = \ case
  BoolValue v -> pure v
  other -> abort $ mconcat
    [ "expected boolean but got "
    , Text.pack $ show other
    ]

assertAnd :: Value -> Eval (Value, Value)
assertAnd = \ case
  AndValue v1 v2 -> pure (v1, v2)
  other -> abort $ mconcat
    [ "expected 'and' value but got "
    , Text.pack $ show other
    ]

assertOr :: Value -> Eval (OrTag, Value)
assertOr = \ case
  OrValue d v -> pure (d, v)
  other -> abort $ mconcat
    [ "expected 'or' value but got "
    , Text.pack $ show other
    ]

assertWith :: Value -> Eval (Env, Expr, Expr)
assertWith = \ case
  WithValue env a b -> pure (env, a, b)
  other -> abort $ mconcat
    [ "expected 'with' value but got "
    , Text.pack $ show other
    ]

assertPar :: Value -> Eval (Env, Expr, Expr)
assertPar = \ case
  ParValue env a b -> pure (env, a, b)
  other -> abort $ mconcat
    [ "expected 'par' value but got "
    , Text.pack $ show other
    ]

assertImplies :: Value -> Eval (Env, Bind, Expr)
assertImplies = \ case
  ImpliesValue env x e -> pure (env, x, e)
  other -> abort $ mconcat
    [ "expected 'and' pair but got "
    , Text.pack $ show other
    ]

data AbortException = AbortException !Text
  deriving stock (Generic, Show)
  deriving anyclass (Exception)

data InternalError = InternalError !Text
  deriving stock (Generic, Show)
  deriving anyclass (Exception)

binding :: Name -> Value -> Eval a -> Eval a
binding x v = locally (\ env -> env { envBinds = (x, v) : envBinds env })

getEnv :: Eval Env
getEnv = Eval ask

getsEnv :: (Env -> a) -> Eval a
getsEnv = Eval . asks

locally :: (Env -> Env) -> Eval a -> Eval a
locally f e = Eval . ReaderT $ \ env -> MaybeT $ runEval e $ f env

raise :: Exception e => e -> Eval a
raise = liftIO . throwIO

envLookup :: Name -> Eval Value
envLookup x = do
  env <- getEnv
  case lookup x $ envBinds env of
    Just v -> pure v
    Nothing -> raise $ InternalError $ mconcat
      [ "unbound variable "
      , nameText x
      ]

execute :: Program -> IO (Maybe Value)
execute program = do
  (env, task) <- ioEnv stdout
  result <- flip runEval env $ go $ programStatements program
  -- This will hang if the output channel isn't closed.
  wait task
  pure result
  where
    go (Define (Bind name _) body : statements) = do
      definition <- eval body
      binding name definition $ go statements
    go [] = do
      (staticEnv, Bind x _t, e) <- assertImplies =<< envLookup (Name "main")
      result <- locally (const staticEnv) $ binding x UnitValue $ eval e
      closeOutputChannel
      pure result

ioEnv :: Handle -> IO (Env, Async ())
ioEnv out = do
  chan <- newChan
  task <- async let
    loop = readChan chan >>= \ case
      Nothing -> pure ()
      Just value -> do
        hPrint out value
        loop
    in loop
  pure
    ( Env
      { envBinds = []
      , envOutputChannel = Just chan
      }
    , task
    )

initialEnv :: Env
initialEnv = Env
  { envBinds = []
  , envOutputChannel = Nothing
  }

abort :: Text -> Eval a
abort = raise . AbortException

-- | Execute an n-ary multiplicative disjunction elimination.
--
--   * If any task aborts, abort.
--
--   * If all tasks are done, done.
--
--   * If exactly one task returns, return.
--
--   * If multiple tasks return, error.
--
oneOfMany :: forall a. [Async (Maybe a)] -> IO (Either SomeException (Maybe a))
oneOfMany asyncs = loop
  where

    loop :: IO (Either SomeException (Maybe a))
    loop = do
      statuses <- for asyncs poll
      let exceptions = [exception | Just (Left exception) <- statuses]
      case exceptions of

        -- At least one task failed: cancel all tasks and fail.
        exception : _ -> do
          for_ asyncs cancel
          pure (Left exception)

        -- No tasks have yet failed.
        [] -> let

          -- Collect results of all tasks.
          allResults :: Maybe [Maybe a]
          allResults = for statuses \ status -> do
            Right result <- status
            pure result

          in case allResults of

            -- If at least one task hasn't completed, continue polling.
            Nothing -> loop

            -- All tasks have completed.
            Just results -> case catMaybes results of

              -- If exactly one task returned a result, return.
              [value] -> pure (Right (Just value))

              -- If no task returned a result (or there are no tasks), done.
              [] -> pure (Right Nothing)

              -- If multiple tasks returned, error.
              _ -> throwIO (InternalError "at most one task should return")

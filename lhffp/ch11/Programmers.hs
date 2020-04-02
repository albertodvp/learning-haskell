data OS = Linux | Windows | Mac
  deriving Show

data ProgLang = Haskell | Python | Rust | Go
  deriving Show

data Programmer =
  Programmer { os :: OS,
               lang :: ProgLang
             }
  deriving Show

oss = [Linux, Windows, Mac]
pls = [Haskell, Python, Rust, Go]

allProgs :: [Programmer]
allProgs = [
  Programmer { os = ops, lang = pl }
    | ops <- oss, pl <- pls
  ]


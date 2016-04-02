import Test.DocTest

main :: IO ()
main = doctest ["-XFlexibleContexts", "src/"]

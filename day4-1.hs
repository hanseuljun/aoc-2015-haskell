import Crypto.Hash (hash, Digest, MD5)
import Data.ByteString.Char8 (pack)
import Data.List (find, isPrefixOf)

md5Hash :: String -> String
md5Hash s = show (hash (pack s) :: Digest MD5)

startsWithFiveZeros :: String -> Bool
startsWithFiveZeros s = "00000" `isPrefixOf` s

main :: IO ()
main = do
    -- putStrLn (md5Hash "abcdef609043")
    -- print (startsWithFiveZeros (md5Hash "abcdef609043"))
    let answer = find (\x -> startsWithFiveZeros (md5Hash ("yzbqklnj" ++ (show x)))) [0..]
    print answer

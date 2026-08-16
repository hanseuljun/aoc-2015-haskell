import Crypto.Hash (hash, Digest, MD5)
import Data.ByteString.Char8 (pack)
import Data.List (find, isPrefixOf)

md5Hash :: String -> String
md5Hash s = show (hash (pack s) :: Digest MD5)

startsWithSixZeros :: String -> Bool
startsWithSixZeros s = "000000" `isPrefixOf` s

main :: IO ()
main = do
    let answer = find (\x -> startsWithSixZeros (md5Hash ("yzbqklnj" ++ (show x)))) [0..]
    print answer

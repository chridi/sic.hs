
{-
 - FILE:    sic.hs
 - AUTHOR:  chridi <chridi@lavabit.com>
 - VERSION: 0.1
 -
 - This is a sic clone written in haskell.  The original sic can be
 - found at [1].
 -
 - [1] http://tools.suckless.org/sic
 -}


import Network
import System
import System.IO
import System.Exit
import Control.Concurrent


{-
 - Some default settings.
 -}
defaultHost = "irc.freenode.net"
defaultPort = "6667"
defaultNick = "sic_hs"


{-
 - Functions for parsing of the command line arguments.
 -}
parseArgs :: [String] -> String -> Maybe String
parseArgs (x:y:xs) arg
        | x == arg = Just y
        | otherwise = parseArgs (y:xs) arg
parseArgs _ arg = Nothing


maybeDefault :: Maybe String -> String -> String
maybeDefault (Just x) y = x
maybeDefault Nothing x  = x


{-
 - The Main function parses the command line arguments and connects to
 - the server.  After this it will fork the receiving function and call
 - the user input handler.
 -}
main = do
        args <- getArgs

        let may = map (parseArgs args) ["-h", "-p", "-n", "-k"]

        let host = maybeDefault (may !! 0) defaultHost
        let port = maybeDefault (may !! 1) defaultPort
        let nick = maybeDefault (may !! 2) defaultNick
        let key  = maybeDefault (may !! 3) ""

        h <- connectTo host (PortNumber $ fromIntegral (read port :: Int))
        hSetBuffering h NoBuffering

        --  Send login information
        if not $ null key then hPutStrLn h ("PASS " ++ key) else return ()
        hPutStrLn h $ unwords ["NICK", nick]
        hPutStrLn h $ unwords ["USER", nick, "0 * :sic.hs"]

        rid <- forkIO $ (recvLoop h) `catch` (\e -> exitSuccess)
        (sendLoop h "" nick) `catch` (\e -> exitSuccess)


{-
 - The receive loop handles the input from the server.
 -}
recvLoop :: Handle -> IO ()
recvLoop h = do
        line <- hGetLine h

        let w = words line

        if head w == "PING"
                then hPutStrLn h $ unwords ["PONG", drop 5 line]
                else if w !! 1 == "PRIVMSG"
                        then putStrLn $ unwords
                                [ w !! 2
                                , "<" ++ (drop 1 $ takeWhile (\x -> x /= '!') $ head w) ++ ">"
                                , drop 1 $ w !! 3
                                ]
                        else putStrLn line
                

        recvLoop h


{-
 - The send loop handles the user input.
 -}
sendLoop :: Handle -> String -> String -> IO ()
sendLoop h chan nick = do
        line <- getLine

        case (take 3 line) of
                ":j " -> do
                                hPutStrLn h $ unwords ["JOIN", drop 3 line]
                                sendLoop h (drop 3 line) nick

                ":l " -> hPutStrLn h $ unwords ["PART", drop 3 line]

                ":q"  -> do
                                hPutStrLn h $ unwords ["QUIT", ":I <3 sic.hs"]
                                putStrLn "sent quit message..."
                                hFlush h
                                hClose h
                                exitSuccess

                ":m " -> hPutStrLn h $ unwords  [ "PRIVMSG"
                                                , (head $ words $ drop 3 line)
                                                , ":" ++ (unlines $ drop 2 $ words line)
                                                ]

                ":s " -> sendLoop h (drop 3 line) nick

                _     -> do
                                hPutStrLn h $ unwords ["PRIVMSG", chan, ":" ++ line]
                                putStrLn $ unwords [chan, "<" ++ nick ++ ">", line]

        sendLoop h chan nick

--  vim:et:tw=72:

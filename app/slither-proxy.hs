import SlitherBot.Proxy
import SlitherBot.Prelude

main :: IO ()
main = runStdoutLoggingT (filterLogger (\_ ll -> ll >= LevelInfo) (proxy 1337))

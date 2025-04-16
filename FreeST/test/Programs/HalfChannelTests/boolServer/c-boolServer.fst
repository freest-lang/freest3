type BoolClient = +{ And: Skip; !Bool; !Bool; ?Bool
                        , Or : Skip; !Bool; !Bool; ?Bool
                        , Not: Skip; !Bool; ?Bool
                        }
                        ; Close

client1 : BoolClient -> Bool
client1 w = w |> select And
              |> send True  
              |> send False
              |> receiveAndClose @Bool 


startClient : (BoolClient -> Bool) -> Bool
startClient client =
  newHcClient1 @BoolClient ("127.0.0.1", "8081") |>
  client 

main : Bool
main = startClient client1

-- remove skips from the end
-- Type check : environment checks only the linear part (filter)

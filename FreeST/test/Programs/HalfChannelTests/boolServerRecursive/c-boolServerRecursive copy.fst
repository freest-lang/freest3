type BoolClient = +{  And : !Bool; !Bool; ?Bool; BoolClient
                  ,   Or  : !Bool; !Bool; ?Bool; BoolClient
                  ,   Not : !Bool; ?Bool; BoolClient
                  ,   Done: Close
                  }

client1 : BoolClient -> Bool
client1 c =
  let (x, c) = 
    select And c
    |> send True
    |> send True
    |> receive in
  print @Bool x ; 
  let (y, c) = 
    select Not c
    |> send x 
    |> receive in
  select Done c |> close ;
  y

main : Bool
main =
  newHcClient1 @BoolClient ("127.0.0.1", "8081") |>
  client1

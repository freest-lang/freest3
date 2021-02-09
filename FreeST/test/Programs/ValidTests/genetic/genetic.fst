{-

This program is an implementation of a Genetic Algorithm tailored
  especifically for the All Ones problem (number of ones in the
  binary representation).

Additional notes:
  - to enable printing inside the sequential genetic algorithm,
      uncomment lines 242 and 244

-}


-- ===== MAIN =====

main : ()
main =
  let result = clientParallel in
  -- Print value
  printChar 'R'; printIntLn result;
  -- Print fitness
  printChar 'F'; printIntLn $ fitnessAllOnes result

-- Example of a client using the sequential genetic algorithm
clientSequential : Int
clientSequential =
  -- User defined metrics
  let seed = 19237 in
  let popSize = 15 in
  let iterations = 100 in
  -- Run genetic algorithm
  geneticAlg seed popSize iterations

-- Example of a client using the parallel genetic algorithm
clientParallel : Int
clientParallel =
  -- User defined metrics
  let seed    = 12345 in
  let islands = 4  in
  let popSize = 10 in
  let nIterI  = 5  in
  let nIterG  = 10 in
  -- Initialize all needed processes
  let resultC = initIslands seed islands popSize nIterI nIterG in
  -- Start computation & receive the result
  let (result, _) = receive resultC in
  result


-- ===== CONSTANTS =====

-- Number of bits of the numbers in the individuals
numBits : Int
numBits = 62


-- ===== UTILITY =====

-- == RANDOM ==

-- Generates a pseudorandom number given a seed, and an upper bound
getNextRand : Int -> Int -> (Int, Int)
getNextRand seed upperBound =
  let random = nextRandom seed in
  (random, mod random upperBound)

-- Generates a pseudorandom number given a seed (using linear congruential
--   generation)
nextRandom : Int -> Int
nextRandom seed =
  let a = 1103515245 in
  let c = 12345 in
  let m = 9223372036854775808 in -- 1 << 63
  abs $ mod (a * seed + c) m

-- == MATH ==

-- Counts the amount of ones in the binary representation of a number
countOnes : Int -> Int
countOnes i =
  if i/2 > 0
  then (mod i 2) + countOnes (i/2)
  else mod i 2


-- Calculates a power
pow : Int -> Int -> Int
pow base exp =
  if exp == 0
  then 1
  else base * (pow base (exp-1))


-- ===== INDIVIDUAL =====

-- Represents a single individual, in this case, a single Integer
type Individual = Int


-- Generate a random individual given a seed
--   This implementation generates an individual
--   with at most 2^3 as its value
generateIndividual : Int -> (Int, Individual)
generateIndividual seed = getNextRand seed (pow 2 3)


-- Compares two individuals and returns the fittest
compareIndividuals : Individual -> Individual -> Individual
compareIndividuals ind0 ind1 =
  if fitnessAllOnes ind0 >= fitnessAllOnes ind1
  then ind0
  else ind1


-- Fitness function for the AllOnes problem (amount of 1s in binary rep)
fitnessAllOnes : Individual -> Int
fitnessAllOnes = countOnes


-- Crossover an individual with another individual using a seed, and return a
--   new individual alongside a new seed to use
crossoverWith : Int -> Individual -> Individual -> (Int, Individual)
crossoverWith seed ind0 ind1 =
  let (seed, inds) = crossoverBoth seed ind0 ind1 in
  let (_, ind1) = inds in
  (seed, ind1)


-- Performs a random crossover between two individuals using a random
--   crossover point (generated using a seed), and returns the individuals
--   and a new seed
crossoverBoth : Int -> Individual -> Individual -> (Int, Individual, Individual)
crossoverBoth seed ind0 ind1 =
  -- Generate random crossover point (at most to the last bit possible)
  let (seed, crossover) = getNextRand seed numBits in
  -- Return result with an updated seed
  (seed, crossoverBoth_ ind0 ind1 1 (crossover+1))

crossoverBoth_ : Individual -> Individual -> Int -> Int -> (Individual, Individual)
crossoverBoth_ ind0 ind1 counter crossover =
  if crossover == 0
  then
    (ind0 * counter, ind1 * counter)
  else
    let bit0 = mod ind0 2 in
    let bit1 = mod ind1 2 in
    let (ind0, ind1) = crossoverBoth_ (ind0/2) (ind1/2) (counter*2) (crossover-1) in
    ((bit1 * counter) + ind0, (bit0 * counter) + ind1)


-- Mutates an individual given a seed and returns the mutated individual and
--   a new seed.
--   This implementation sums a random power of 2 and does a wraparound
mutateIndividual : Int -> Individual -> (Int, Individual)
mutateIndividual seed ind =
  let (seed, exp) = getNextRand seed (numBits+1) in
  (seed, mod (ind + pow 2 exp) (pow 2 numBits))


-- ===== POPULATION =====

-- Represents a population a.k.a. a list of Individuals
data Population = NilPop | ConsPop Individual Population


-- Generate a random population of a given size using a given seed
generatePopulation : Int -> Int -> (Int, Population)
generatePopulation seed size =
  if size == 0
  then (seed, NilPop)
  else
    let (seed, individual) = generateIndividual seed in
    let (seed, pop) = generatePopulation seed (size-1) in
    (seed, ConsPop individual pop)


-- Get the fittest individual of a population and isolates them from the rest
getFittestIndividual : Population -> (Individual, Population)
getFittestIndividual population =
  case population of {
    NilPop -> (0, NilPop),  -- Should never happen
    ConsPop ind l ->
      getIndividual_ ind l (\x:Int -> (\y:Int -> x >= y))
  }

-- Get the fittest individual of a population and isolates them from the rest
getUnFittestIndividual : Population -> (Individual, Population)
getUnFittestIndividual population =
  case population of {
    NilPop -> (0, NilPop),   -- Should never happen
    ConsPop ind l ->
      getIndividual_ ind l (\x:Int -> (\y:Int -> x < y))
    }

-- Auxiliary function to filter through a population
--   Uses a function to compare individuals' fitness
getIndividual_ : Individual -> Population -> (Individual -> Individual -> Bool) -> (Individual, Population)
getIndividual_ ind0 pop0 f =
  case pop0 of {
    NilPop  -> (ind0, NilPop),
    ConsPop ind1 pop1 ->
      let (fittest, pop) = getIndividual_ ind0 pop1 f in
      if f (fitnessAllOnes fittest) (fitnessAllOnes ind1)
      then (fittest, ConsPop ind1 pop)
      else (ind1, ConsPop fittest pop)
  }


-- Crossover a population with an individual given a seed
crossoverPopulation : Int -> Individual -> Population -> (Int, Population)
crossoverPopulation seed ind0 pop0 =
  case pop0 of {
    NilPop -> (seed, NilPop),
    ConsPop ind1 pop1 ->
      let (seed, ind1) = crossoverWith seed ind0 ind1 in
      let (seed, pop) = crossoverPopulation seed ind0 pop1 in
      (seed, ConsPop ind1 pop)
  }


-- Mutate all individuals in a population
mutatePopulation : Int -> Population -> (Int, Population)
mutatePopulation seed pop0 =
  case pop0 of {
    NilPop -> (seed, NilPop),
    ConsPop ind1 pop1 ->
      let (seed, ind1) = mutateIndividual seed ind1 in
      let (seed, pop) = mutatePopulation seed pop1 in
      (seed, ConsPop ind1 pop)
  }


-- ===== SEQUENTIAL GENETIC ALGORITHM =====

-- Sequential genetic algorithm
geneticAlg : Int -> Int -> Int -> Individual
geneticAlg seed populationSize iterations =
  -- Generate first population
  let (seed, pop) = generatePopulation seed populationSize in
  -- Get first fittest individual
  --let (fittest, _) = getFittestIndividual pop in
  -- Print out first individual
  --printIntLn fittest;
  -- Apply the genetic algorithm
  let (_, pop) = geneticAlg_ seed iterations pop in
  -- Get the resulting population's fittest individual
  let (fittest, _) = getFittestIndividual pop in
  -- Return it
  fittest

geneticAlg_ : Int -> Int ->  Population -> (Int, Population)
geneticAlg_ seed iterations pop =
  if iterations == 0
  then (seed, pop)
  else
    -- Selection - isolate fittest individual from the population
    let (fittest, pop) = getFittestIndividual pop in
    -- Crossover - crossover every individual with the fittest individual
    let (seed, pop) = crossoverPopulation seed fittest pop in
    -- Mutation - randomly mutate the population (not the fittest individual)
    let (seed, pop) = mutatePopulation seed pop in
    -- Compute fitness of the (hopefully) new fittest individual
    --let (fittest, _) = getFittestIndividual pop in
    -- Print information
    --printIntLn fittest;
    -- Re-add the fittest individual & Continue the algorithm (-1 iteration)
    geneticAlg_ seed (iterations - 1) (ConsPop fittest pop)


-- ===== PARALLEL GENETIC ALGORITHM - MASTER ISLANDS =====

-- Channel to communicate to islands
type IslandChannel : SL = +{
  Fittest:   ?Int; IslandChannel, -- Gets the fittest individual of an Island
  Crossover: !Int; IslandChannel, -- Sends an individual to perform a GA iteration
  End:       ?Int }               -- Equal to Fittest but it closes the channel


-- Channel for the client to ask master the result
type ResultChannel : SL = ?Int    -- Compute result and return it


-- Structure that represents a list of IslandChannels
--   Used by the master to hold all channels to the islands
data ListIslandChannel = Nil | Cons IslandChannel ListIslandChannel


-- Initialize all needed processes (islands + master) and return a
--   ResultChannel for the client to request the result
initIslands : Int -> Int -> Int -> Int -> Int -> ResultChannel
initIslands = initIslands_ Nil

initIslands_ : ListIslandChannel -> Int -> Int -> Int -> Int -> Int -> ResultChannel
initIslands_ channels seed islands popSize nIterI nIterG =
  if islands == 0
  then
    let (client, server) = new ResultChannel in
    fork $ runMasterServer server channels nIterG;
    client
  else
    let (master, island) = new IslandChannel in
    let (seed, pop) = generatePopulation seed popSize in
    fork $ runIsland island seed nIterI pop;
    initIslands_ (Cons master channels) seed (islands-1) popSize nIterI nIterG


-- Run the master process that coordinates all the islands
--   and then sends the result to the client
runMasterServer : dualof ResultChannel -> ListIslandChannel -> Int -> ()
runMasterServer c channels nIterG =
  -- Apply nIterG global iterations
  let channels = masterLoop channels nIterG in
  -- End and commpute fittest individual from all islands
  --   and send it to the client
  let _ =  send (endIslands channels) c in
  -- Stop
  ()

-- Auxiliary function that performs the getFittest-sendFittest loop
masterLoop : ListIslandChannel -> Int -> ListIslandChannel
masterLoop channels nIterG =
  if nIterG == 0
  then
    channels
  else
    -- Get fittest from all
    let (fittest, channels) = receiveFittest channels in
    -- Print information
    --printIntLn fittest;
    -- Send fittest to all
    let channels = sendFittest channels fittest in
    -- Continue iterating
    masterLoop channels (nIterG-1)


-- Run an island instance that holds a population an performs
--   the GA on demand (by the master)
runIsland : dualof IslandChannel -> Int -> Int -> Population -> ()
runIsland master seed nIterI pop =
  match master with {
    Fittest master ->
      -- Get our population's fittest
      let (ourFittest, _) = getFittestIndividual pop in
      -- Send it to the master
      let master = send ourFittest master in
      -- Continue serving
      runIsland master seed nIterI pop,
    Crossover master ->
      -- Receive true fittest
      let (fittest, master) = receive master in
      -- Kill unfittest from population...
      let (_, pop) = getUnFittestIndividual pop in
      -- ... and insert the true fittest in the population
      let pop = ConsPop fittest pop in
      -- Apply GA nIterI times inside island
      let (seed, pop) = geneticAlg_ seed nIterI pop in
      -- Continue serving
      runIsland master seed nIterI pop,
    End master ->
      -- Get our population's fittest
      let (ourFittest, _) = getFittestIndividual pop in
      -- Send it to the master
      let _ = send ourFittest master in
      -- Stop (get some help  -Michael Jordan)
      ()
  }


-- Compute the absolute fittest individual of all islands
receiveFittest : ListIslandChannel -> (Individual, ListIslandChannel)
receiveFittest channels0 =
  case channels0 of {
    Nil ->
      (0, Nil), -- This case should never happen, there is always at least one island
    Cons channel channels1 ->
      -- Receive first island's fittest individual...
      let (fittest, channel) = receive $ select Fittest channel in
      -- ... to compare with fittest individuals from other islands
      let (fittest, channels1) = receiveFittest_ channels1 fittest in
      -- Return fittest individual and the rebuilt island channel list
      (fittest, Cons channel channels1)
  }

receiveFittest_ : ListIslandChannel -> Individual -> (Individual, ListIslandChannel)
receiveFittest_ channels0 fittest0 =
  case channels0 of {
    Nil ->
      (fittest0, Nil),
    Cons channel channels1 ->
      -- Receive island's fittest individual...
      let (fittest1, channel) = receive $ select Fittest channel in
      -- Compare fittests and get the fittest
      let fittest = compareIndividuals fittest0 fittest1 in
      -- Continue comparing to fittest individuals from other islands
      let (fittest, channels1) = receiveFittest_ channels1 fittest in
      -- Return fittest individual from all islands and the channels
      (fittest, Cons channel channels1)
  }


-- Send an individual to every island to do another round of the GA
sendFittest : ListIslandChannel -> Individual -> ListIslandChannel
sendFittest channels0 fittest =
  case channels0 of {
    Nil ->
      Nil,
    Cons channel channels1 ->
      let channel = send fittest $ select Crossover channel in
      Cons channel $ sendFittest channels1 fittest
  }


-- End all islands and compute the absolute fittest individual
endIslands : ListIslandChannel -> Individual
endIslands channels0 =
  case channels0 of {
    Nil ->
      0, -- This case should never happen, there is always at least one island
    Cons channel channels1 ->
      -- Receive first island's fittest individual...
      let (fittest, _) = receive $ select End channel in
      -- ... to compare with fittest individuals from other islands
      --     and return the absolute fittest individual
      endIslands_ channels1 fittest
  }

endIslands_ : ListIslandChannel -> Individual -> Individual
endIslands_ channels0 fittest0 =
  case channels0 of {
    Nil ->
      fittest0,
    Cons channel channels1 ->
      -- Receive island's fittest individual...
      let (fittest1, _) = receive $ select End channel in
      -- Compare fittests and get the fittest
      let fittest = compareIndividuals fittest0 fittest1 in
      -- Continue comparing to fittest individuals from other islands
      endIslands_ channels1 fittest
  }

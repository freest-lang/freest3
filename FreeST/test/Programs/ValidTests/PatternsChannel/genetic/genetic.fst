{-

This program is an implementation of a Genetic Algorithm tailored
  especifically for the All Ones problem (number of ones in the
  binary representation).

Additional notes:
  - to enable printing inside the sequential genetic algorithm,
      uncomment lines 238 and 240
  - to enable printing inside the parallel genetic algorithm,
      uncomment line 327

-}


-- ===== MAIN =====

main : ()
main =
  let result = clientParallel in
  -- Print value
  printString "  Value: "; printIntLn result;
  -- Print fitness
  printString "Fitness: "; printIntLn $ fitnessAllOnes result

-- Initial seed for random number generation
argSeed : Int
argSeed = 12345

-- Amount of individuals per population
argPopSize : Int
argPopSize = 10

-- Iterations per population
argIterPop : Int
argIterPop = 5

-- [PARALLEL] Amount of islands (each island has one population)
argIslands : Int
argIslands = 4

-- [PARALLEL] Amount of population iteration & fittest individual sync
argIterIsl : Int
argIterIsl = 5

-- Example of a client using the sequential genetic algorithm
clientSequential : Int
clientSequential = geneticAlg argSeed argPopSize argIterPop

-- Example of a client using the parallel genetic algorithm
clientParallel : Int
clientParallel = fst@Int@Skip $ receive $ initIslands argSeed argIslands argPopSize argIterPop argIterIsl


-- ===== CONSTANTS =====

-- Max number
maxNum : Int
maxNum = 100000


-- ===== UTILITY =====

-- == RANDOM ==

-- Generates a pseudorandom number given a seed, and an upper bound
nextRandomBounded : Int -> Int -> (Int, Int)
nextRandomBounded seed upperBound =
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


-- ===== INDIVIDUAL =====

-- Represents a single individual, in this case, a single Integer
type Individual = Int


-- Generate a random individual given a seed
--   This implementation generates an individual
--   with at most 2^3 as its value
generateIndividual : Int -> (Int, Individual)
generateIndividual seed = nextRandomBounded seed (2^3)


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
  -- Generate random crossover point (at most to the maxNum)
  let (seed, crossover) = nextRandomBounded seed maxNum in
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
    let (ind0, ind1) = crossoverBoth_ (ind0/2) (ind1/2) (counter*2) (crossover/2) in
    ((bit1 * counter) + ind0, (bit0 * counter) + ind1)


-- Mutates an individual given a seed and returns the mutated individual and
--   a new seed.
--   This implementation sums a random power of 2 and does a wraparound
mutateIndividual : Int -> Individual -> (Int, Individual)
mutateIndividual seed ind =
  let (seed, n) = nextRandomBounded seed maxNum in
  (seed, mod (ind + n) (maxNum))


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
getFittestIndividual NilPop          = (0, NilPop) -- Should never happen
getFittestIndividual (ConsPop ind l) = getIndividual_ ind l (>=)

-- Get the fittest individual of a population and isolates them from the rest
getUnFittestIndividual : Population -> (Individual, Population)
getUnFittestIndividual NilPop          = (0, NilPop) -- Should never happen
getUnFittestIndividual (ConsPop ind l) = getIndividual_ ind l (<)

-- Auxiliary function to filter through a population
--   Uses a function to compare individuals' fitness
getIndividual_ : Individual -> Population -> (Individual -> Individual -> Bool) -> (Individual, Population)
getIndividual_ ind0 NilPop f = (ind0, NilPop)
getIndividual_ ind0 (ConsPop ind1 pop1) f =
  let (fittest, pop) = getIndividual_ ind0 pop1 f in
  if f (fitnessAllOnes fittest) (fitnessAllOnes ind1)
  then (fittest, ConsPop ind1 pop)
  else (ind1, ConsPop fittest pop)

-- Crossover a population with an individual given a seed
crossoverPopulation : Int -> Individual -> Population -> (Int, Population)
crossoverPopulation seed ind0 NilPop = (seed, NilPop)
crossoverPopulation seed ind0 (ConsPop ind1 pop1) =
  let (seed, ind1) = crossoverWith seed ind0 ind1 in
  let (seed, pop) = crossoverPopulation seed ind0 pop1 in
  (seed, ConsPop ind1 pop)

-- Mutate all individuals in a population
mutatePopulation : Int -> Population -> (Int, Population)
mutatePopulation seed NilPop = (seed, NilPop)
mutatePopulation seed (ConsPop ind1 pop1) = 
  let (seed, ind1) = mutateIndividual seed ind1 in
  let (seed, pop) = mutatePopulation seed pop1 in
  (seed, ConsPop ind1 pop)

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
type IslandChannel : 1S = +{
  Fittest:   ?Int; IslandChannel, -- Gets the fittest individual of an Island
  Crossover: !Int; IslandChannel, -- Sends an individual to perform a GA iteration
  EndC:       Skip }               -- Close the channel


-- Channel for the client to ask master the result
type ResultChannel : 1S = ?Int    -- Compute result and return it


-- Structure that represents a list of IslandChannels
--   Used by the master to hold all channels to the islands
data ListIslandChannel : 1T = Nil() | Cons IslandChannel ListIslandChannel


-- Initialize all needed processes (islands + master) and return a
--   ResultChannel for the client to request the result
initIslands : Int 1-> Int 1-> Int 1-> Int 1-> Int 1-> ResultChannel
initIslands = initIslands_ $ Nil()

initIslands_ : ListIslandChannel -> Int 1-> Int 1-> Int 1-> Int 1-> Int 1-> ResultChannel
initIslands_ channels seed islands popSize nIterI nIterG =
  if islands == 0
  then
    let (client, server) = new ResultChannel in
    fork (\_:() 1-> runMasterServer server channels nIterG);
    client
  else
    let (master, island) = new IslandChannel in
    let (seed, pop) = generatePopulation seed popSize in
    fork (\_:() 1-> runIsland island seed nIterI pop);
    initIslands_ (Cons master channels) seed (islands-1) popSize nIterI nIterG


-- Run the master process that coordinates all the islands
--   and then sends the result to the client
runMasterServer : dualof ResultChannel -> ListIslandChannel 1-> Int 1-> ()
runMasterServer c channels nIterG =
  -- Apply nIterG global iterations
  let channels = masterLoop channels nIterG in
  -- Get fittest individual from all islands...
  let (fittest, channels) = receiveFittest channels in
  -- ... and send it to the client
  let _ = send fittest c in
  -- End all islands
  endIslands channels

-- Auxiliary function that performs the getFittest-sendFittest loop
masterLoop : ListIslandChannel -> Int 1-> ListIslandChannel
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
    let channels = sendFittest fittest channels in
    -- Continue iterating
    masterLoop channels (nIterG-1)


-- Run an island instance that holds a population an performs
--   the GA on demand (by the master)
runIsland : dualof IslandChannel -> Int 1-> Int 1-> Population 1-> ()
runIsland (Fittest master) seed nIterI pop =
  -- Get our population's fittest
  let (ourFittest, _) = getFittestIndividual pop in
  -- Send it to the master
  let master = send ourFittest master in
  -- Continue serving
  runIsland master seed nIterI pop
runIsland (Crossover master) seed nIterI pop =
  -- Receive true fittest
  let (fittest, master) = receive master in
  -- Kill unfittest from population...
  let (_, pop) = getUnFittestIndividual pop in
  -- ... and insert the true fittest in the population
  let pop = ConsPop fittest pop in
  -- Apply GA nIterI times inside island
  let (seed, pop) = geneticAlg_ seed nIterI pop in
  -- Continue serving
  runIsland master seed nIterI pop
runIsland (EndC master) seed nIterI pop = 
  -- Stop (get some help  -Michael Jordan)
  ()

-- Compute the absolute fittest individual of all islands
receiveFittest : ListIslandChannel -> (Individual, ListIslandChannel)
receiveFittest = foldIslands@Individual receiveFittestF 0

receiveFittestF :  Individual -> IslandChannel -> (Individual, IslandChannel)
receiveFittestF ind0 island =
  let (ind1, island) = receive $ select Fittest island in
  (compareIndividuals ind0 ind1, island)


-- Send an individual to every island to do another round of the GA
sendFittest : Individual -> ListIslandChannel -> ListIslandChannel
sendFittest fittest channels0 = snd@Individual@ListIslandChannel $ foldIslands@Int sendFittestF fittest channels0

sendFittestF :  Int -> IslandChannel -> (Int, IslandChannel)
sendFittestF fittest island = (fittest, send fittest $ select Crossover island)


-- End all islands and compute the absolute fittest individual
--   Note: This function can not use the foldIslands function because
--         the resulting channels would be Skip and not IslandChannel,
--         therefore raising an error
endIslands : ListIslandChannel -> ()
endIslands (Nil _) = ()
endIslands (Cons channel channels1) =
  let _ = select EndC channel in
  endIslands channels1

-- Fold function over a list of IslandChannels
foldIslands : forall a . (a -> IslandChannel -> (a, IslandChannel)) -> a -> ListIslandChannel -> (a, ListIslandChannel)
foldIslands f x (Nil _) = (x, Nil())
foldIslands f x (Cons ch chss) = 
  let (x, ch) = f x ch in
  let (x, chss) = foldIslands@a f x chss in
  (x, Cons ch chss)

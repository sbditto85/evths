module Main where

consumer :: IO Consumer
consumer = makeConsumer $ do

  category "testCategory"

  -- optional
  store daStore
  clock daClock
  -- endOptional

  handle "Open" $ \open -> do
    let testId' = open ^. testId
    test <- fetch testId'

    if not $ testIdempotent test then
      log ["ignore"] "this has already happened"
    else do
      let next = follow open & theOtherThing .~ (open ^. otherThing)

      write next testId'
  
  handle "Next" $ \next -> do
    let testId' = next ^. testId
    (test, version) <- fetchVersion testId'

    if not $ testIdempotent test then
      log ["ignore"] "this has already happened"
    else do
      let last = follow next

      writeVersion last testId' version

run :: IO ()
run = do
  runConsumer consumer consumerSettings

runMultiple :: IO ()
runMultiple = do
  runConsumers 
    [ consumerOne consumerOneSettings
    , consumerTwo consumerTwoSettings
    ]

main :: IO ()
main = do
  putStrLn "hello world"

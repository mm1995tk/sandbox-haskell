{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Examples.HasqlExample where

import Control.Concurrent.Async (concurrently)
import Data.Int (Int32)
import Data.Profunctor (lmap)
import Data.Text (Text)
import Data.Time
import Data.Vector (Vector, fromList, snoc)
import Hasql.Connection hiding (acquire, release)
import Hasql.Pool
import Hasql.Session
import Hasql.Statement (Statement (..))
import Hasql.TH (maybeStatement, resultlessStatement, singletonStatement, vectorStatement)
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions (IsolationLevel (RepeatableRead), Mode (Write), transaction)
import MyLib.Utils ()

data Person = Person
  { personId :: Text
  , age :: Int32
  , fullName :: Text
  }
  deriving (Show)

-- inConnection :: (Pool -> IO a) -> IO a
-- inConnection f = getPool >>= \pool -> f pool <* release' pool

inConnection :: IO (Session a) -> IO (Either UsageError a)
inConnection f = do
  (pool, f') <- concurrently getPool f
  use pool f' <* release' pool

notRelease :: IO (Session a) -> IO (Pool, Either UsageError a)
notRelease f = do
  (pool, f') <- concurrently getPool f
  (pool,) <$> use pool f'

getPool :: IO Pool
getPool = acquire 10 (secondsToDiffTime 30) (secondsToDiffTime 30 * 5) $ settings "localhost" 5431 "postgres" "postgres" "postgres"

-- cleanUpDb :: IO (Either UsageError ())
-- cleanUpDb = inConnection $ statement () $ Statement "truncate person" E.noParams D.noResult True

release' :: Pool -> IO ()
release' c = putStrLn "released" *> release c

multiInsert' :: Statement (Vector Person) ()
multiInsert' =
  encode
    [resultlessStatement|
    insert into 
      person (person_id, age, full_name) 
    select * from unnest($1 :: text[], $2 :: int4[], $3 :: text[] )
    |]
  where
    encode = lmap $ foldl (\(a, b, c) Person{..} -> (snoc a personId, snoc b age, snoc c fullName)) (mempty, mempty, mempty)

multiInsert :: [Person] -> Session ()
multiInsert = flip statement multiInsert' . fromList

multiUpdate :: [Person] -> Session ()
multiUpdate ps =
  statement
    (fromList ps)
    $ encode
      [resultlessStatement|
    update person set 
      person_id = newdata.person_id,
      age = newdata.age,
      full_name = newdata.full_name
    from (select unnest($1 :: text[]) person_id, unnest($2 :: int4[]) age,unnest($3 :: text[]) full_name) newdata
    where
      person.person_id = newdata.person_id
    |]
  where
    encode = lmap $ foldl (\(a, b, c) Person{..} -> (snoc a personId, snoc b age, snoc c fullName)) (mempty, mempty, mempty)

multiUpsert :: [Person] -> Session ()
multiUpsert ps =
  statement
    (fromList ps)
    $ encode
      [resultlessStatement|
    insert into 
      person (person_id, age, full_name) 
    select * from unnest($1 :: text[], $2 :: int4[], $3 :: text[] )
    on conflict (person_id) do update
    set 
      person_id = excluded.person_id,
      age = excluded.age,
      full_name = excluded.full_name
    |]
  where
    encode = lmap $ foldl (\(a, b, c) Person{..} -> (snoc a personId, snoc b age, snoc c fullName)) (mempty, mempty, mempty)

createPerson :: Person -> Session Text
createPerson = flip statement insertProduct
  where
    encode = lmap \Person{..} -> (personId, age, fullName)

    insertProduct :: Statement Person Text
    insertProduct =
      encode
        [singletonStatement|
        insert into 
          person (person_id, age, full_name) 
        values (
          $1 :: text
          , $2 :: int4
          , $3 :: text) 
        returning person_id :: text
      |]

findOne :: Text -> Session (Maybe Person)
findOne =
  flip statement
    $ fmap
      (fmap \(personId, fullName, age) -> Person{..})
      [maybeStatement|
    select 
      person_id :: text
      ,full_name :: text
      , age :: int4 
    from person 
    where person_id = $1 :: text
  |]

findMany :: Int32 -> Session (Vector Person)
findMany =
  flip statement
    $ fmap
      (fmap \(personId, fullName, age) -> Person{..})
      [vectorStatement|
    select 
      person_id :: text
      ,full_name :: text
      , age :: int4 
    from person 
    where age = $1 :: int4
  |]

findMany' :: [Int32] -> Session (Vector Person)
findMany' xs =
  statement (fromList xs)
    $ fmap
      (fmap \(personId, fullName, age) -> Person{..})
      [vectorStatement|
    select 
      person_id :: text
      ,full_name :: text
      , age :: int4 
    from person 
    where age = any($1 :: int4[])
  |]

deleteById :: [Text] -> Session ()
deleteById ids =
  statement
    (fromList ids)
    [resultlessStatement|
    delete from person where person_id = any($1::text[])
  |]

sampleTx :: Tx.Transaction ()
sampleTx = do
  Tx.statement (fromList [Person{fullName = "hgyuf", age = 45, personId = "jdfdfdui"}]) multiInsert'
  Tx.statement
    ( fromList
        [ Person{fullName = "dfghj", age = 29, personId = "poeiyf"}
        , Person{fullName = "fdfdfuuu", age = 65, personId = "gfydee"}
        ]
    )
    multiInsert'

defaultTx :: Tx.Transaction a -> Session a
defaultTx = transaction RepeatableRead Write
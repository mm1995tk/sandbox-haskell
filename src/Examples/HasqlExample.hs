{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Examples.HasqlExample where

import Hasql.Connection hiding (acquire, release)

import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Pool
import Hasql.Session

import Control.Lens (Profunctor (dimap))
import Data.Int (Int32, Int64)
import Data.Profunctor (lmap)
import Data.Text (Text)
import Data.Time
import Data.Vector (Vector, fromList, snoc)
import Hasql.Statement (Statement (..))
import Hasql.TH (foldStatement, maybeStatement, resultlessStatement, singletonStatement, vectorStatement)
import MyLib.Utils ()

data Person = Person
  { personId :: Text
  , age :: Int32
  , fullName :: Text
  }
  deriving (Show)

inConnection :: (Pool -> IO a) -> IO a
inConnection f = getPool >>= \pool -> f pool <* release' pool

getPool :: IO Pool
getPool = acquire 1 (secondsToDiffTime 30) (secondsToDiffTime 30 * 5) $ settings "localhost" 5431 "postgres" "postgres" "postgres"

cleanUpDb :: IO (Either UsageError ())
cleanUpDb = inConnection . flip use $ statement () $ Statement "truncate person" E.noParams D.noResult True

release' :: Pool -> IO ()
release' c = putStrLn "released" *> release c



multiInsert :: [Person] -> Session ()
multiInsert ps =
  statement
    (fromList ps)
    $ encode
      [resultlessStatement|
    insert into 
      person (person_id, age, full_name) 
    select * from unnest($1 :: text[], $2 :: int4[], $3 :: text[] )
    |]
  where
    encode = lmap $ foldl (\(a, b, c) Person{..} -> (snoc a personId, snoc b age, snoc c fullName)) (mempty, mempty, mempty)

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

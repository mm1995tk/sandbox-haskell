{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ApiExample.Infrastructure.Aggregate.Person where

import ApiExample.Domain (Person (..))
import Data.Int (Int32)
import Data.Profunctor (lmap)
import Data.Text (Text)
import Data.Vector (Vector, fromList, snoc)
import Hasql.Session
import Hasql.Statement (Statement (..))
import Hasql.TH (maybeStatement, resultlessStatement, singletonStatement, vectorStatement)
import Hasql.Transaction qualified as Tx
import MyLib.Utils ()

multiInsert' :: Statement (Vector Person) ()
multiInsert' =
  encode
    [resultlessStatement|
    insert into 
      person (person_id, age, full_name) 
    select * from unnest($1 :: text[], $2 :: int4[], $3 :: text[] )
    |]
 where
  encode = lmap $ foldl (\(a, b, c) Person{..} -> (snoc a personId, snoc b (fromIntegral age), snoc c fullName)) (mempty, mempty, mempty)

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
  encode = lmap $ foldl (\(a, b, c) Person{..} -> (snoc a personId, snoc b (fromIntegral age), snoc c fullName)) (mempty, mempty, mempty)

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
  encode = lmap $ foldl (\(a, b, c) Person{..} -> (snoc a personId, snoc b (fromIntegral age), snoc c fullName)) (mempty, mempty, mempty)

createPerson :: Person -> Session Text
createPerson = flip statement insertProduct
 where
  encode = lmap \Person{..} -> (personId, fromIntegral age, fullName)

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
      (fmap \(personId, fullName, age) -> Person{age = fromIntegral age, ..})
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
      (fmap \(personId, fullName, age) -> Person{age = fromIntegral age, ..})
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
      (fmap \(personId, fullName, age) -> Person{age = fromIntegral age, ..})
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

import Data.Either
import Data.Maybe ()
import InMemoryTables qualified as D
import Lib1
import Lib2
import Lib3
import CustomDataTypes qualified as CDTS
import Control.Monad.Free (Free (..), liftF)
import DataFrame qualified as DF (Column (..), ColumnType (..), Value (..), Row, DataFrame (..))
import StackTestRefrenceTables
import YamlHandler qualified as YH
import GeneralConstants qualified as GC
import Test.Hspec
import Data.IORef
import Data.Time (UTCTime)
import YamlHandler (DFExpr(OValue))

inMemoryDatabase :: IO [(String, IORef DF.DataFrame)]
inMemoryDatabase = do
  employees <- newIORef $ snd D.tableEmployees
  flags <- newIORef $ snd D.tableWithNulls
  emptyTable <- newIORef $ DF.DataFrame [DF.Column "byDefaultEmptyRow" DF.StringType] []
  return $ [("employees",employees),("flags",flags),("empty_table",emptyTable)]

getRefFromMemory :: String -> [(String, IORef DF.DataFrame)] -> Maybe (IORef DF.DataFrame)
getRefFromMemory _ [] = Nothing
getRefFromMemory s (x:xs) = 
  if s == fst x
    then Just $ snd x
    else getRefFromMemory s xs 

mapDatabase :: [String] -> [DF.DataFrame] -> [(String, DF.DataFrame)] -> [(String, DF.DataFrame)]
mapDatabase [] [] db = db 
mapDatabase (x1:xs1) (x2:xs2) db = mapDatabase xs1 xs2 $ db++[(x1,x2)]

updateTableInMemory :: DF.DataFrame -> String -> IO ()
updateTableInMemory df str = do 
  db <- inMemoryDatabase
  case getRefFromMemory str db of
    Nothing -> return ()
    Just ref -> writeIORef ref df

getDbFromInMemoryDatabase :: IO (Either CDTS.ErrorMessage Database)
getDbFromInMemoryDatabase = do
  db <- inMemoryDatabase
  let names = fmap (\(name,_) -> name) db
  dfs <- sequence $ fmap (\(_,df) -> readIORef df) db
  let db1 = mapDatabase names dfs []
  return $ Right db1

testTime :: UTCTime
testTime = read "2023-12-12 00:37:56.740852 UTC"

runExecuteIO :: Lib3.Execution r -> IO r
runExecuteIO (Pure r) = return r
runExecuteIO (Free step) = do
    next <- runStep step
    runExecuteIO next
    where
        runStep :: Lib3.ExecutionAlgebra a -> IO a
        runStep (Lib3.GetTime next) = return testTime >>= return . next
        runStep (Lib3.LoadDatabase next) = getDbFromInMemoryDatabase >>= return . next
        runStep (Lib3.WriteOutTable tName df next) = updateTableInMemory df tName >>= return . next

main :: IO ()
main = hspec $ do
  describe "Lib1.findTableByName" $ do
    it "handles empty lists" $ do
      Lib1.findTableByName [] "" `shouldBe` Nothing
    it "handles empty names" $ do
      Lib1.findTableByName D.database "" `shouldBe` Nothing
    it "can find by name" $ do
      Lib1.findTableByName D.database "employees" `shouldBe` Just (snd D.tableEmployees)
    it "can find by case-insensitive name" $ do
      Lib1.findTableByName D.database "employEEs" `shouldBe` Just (snd D.tableEmployees)
  describe "Lib1.parseSelectAllStatement" $ do
    it "handles empty input" $ do
      Lib1.parseSelectAllStatement "" `shouldSatisfy` isLeft
    it "handles invalid queries" $ do
      Lib1.parseSelectAllStatement "select from dual" `shouldSatisfy` isLeft
    it "returns table name from correct queries" $ do
      Lib1.parseSelectAllStatement "selecT * from dual;" `shouldBe` Right "dual"
  describe "Lib1.validateDataFrame" $ do
    it "finds types mismatch" $ do
      Lib1.validateDataFrame (snd D.tableInvalid1) `shouldSatisfy` isLeft
    it "finds column size mismatch" $ do
      Lib1.validateDataFrame (snd D.tableInvalid2) `shouldSatisfy` isLeft
    it "reports different error messages" $ do
      Lib1.validateDataFrame (snd D.tableInvalid1) `shouldNotBe` Lib1.validateDataFrame (snd D.tableInvalid2)
    it "passes valid tables" $ do
      Lib1.validateDataFrame (snd D.tableWithNulls) `shouldBe` Right ()
  describe "Lib1.renderDataFrameAsTable" $ do
    it "renders a table" $ do
      Lib1.renderDataFrameAsTable 100 (snd D.tableEmployees) `shouldSatisfy` not . null
  describe "Lib2.parseStatement" $ do
	  it "handles empty input" $ do
	    Lib2.parseStatement "" `shouldSatisfy` isLeft
	  it "handles bad query statements" $ do
	    Lib2.parseStatement "select from where" `shouldSatisfy` isLeft
	  it "returns correct select statements without a WHERE clause" $ do
	    Lib2.parseStatement "sEleCt * fRom employees;" `shouldBe` Right StackTestRefrenceTables.fun1
	  it "returns correct select statements with a WHERE clause" $ do
	    Lib2.parseStatement "SELECT * FROM table WHERE col1 > col2;" `shouldBe` Right StackTestRefrenceTables.fun2
	  it "returns correct select statements with a WHERE clause and multiple OR statements" $ do
	    Lib2.parseStatement "select * from table where col1 > col2 oR col2 <= col3 or 2 > 1;" `shouldBe` Right StackTestRefrenceTables.fun3
	  it "returns correct select statements with a single column" $ do
	    Lib2.parseStatement "select id from employees;" `shouldBe` Right StackTestRefrenceTables.fun4
	  it "returns a selected table with SHOW" $ do
	    Lib2.parseStatement "SHOW tabLE employees;" `shouldBe` Right StackTestRefrenceTables.fun6
	  it "returns aggregate function statements" $ do
	    Lib2.parseStatement "select MAX(id) from employees;" `shouldBe` Right StackTestRefrenceTables.fun7
  describe "Lib2.parseStatement (again)" $ do -- another describe added because this segment joined to others is classified as a syntax error 
    it "returns all tables from SHOW" $ do
	    Lib2.parseStatement "sHOw tABles;" `shouldBe` Right StackTestRefrenceTables.fun5	
  describe "Lib2.executeStatement" $ do
	  it "recognises only existing tables" $ do
	    Lib2.executeStatement D.database StackTestRefrenceTables.fun2 `shouldSatisfy` isLeft
	  it "recognises only case sensitive table names" $ do
	    Lib2.executeStatement D.database StackTestRefrenceTables.fun20 `shouldSatisfy` isLeft
	  it "recognises only existing columns in tables" $ do
	    Lib2.executeStatement D.database StackTestRefrenceTables.fun8 `shouldSatisfy` isLeft
	  it "recognises only case sensitive column names" $ do
	    Lib2.executeStatement D.database StackTestRefrenceTables.fun21 `shouldSatisfy` isLeft
	  it "recognises non IntegerType provided columns" $ do
	    Lib2.executeStatement D.database StackTestRefrenceTables.fun10 `shouldSatisfy` isLeft
	  it "recognises only existing operators" $ do
	    Lib2.executeStatement D.database StackTestRefrenceTables.fun11 `shouldSatisfy` isLeft
	  it "executes correct select statements" $ do
	    Lib2.executeStatement D.database StackTestRefrenceTables.fun1 `shouldBe` Right StackTestRefrenceTables.fun9
	  it "executes selected columns" $ do
	    Lib2.executeStatement D.database StackTestRefrenceTables.fun12 `shouldBe` Right StackTestRefrenceTables.fun13
	  it "executes select statements with WHERE, OR clauses" $ do
	    Lib2.executeStatement D.database StackTestRefrenceTables.fun14 `shouldBe` Right StackTestRefrenceTables.fun15
	  it "executes MAX aggregate function" $ do
	    Lib2.executeStatement D.database StackTestRefrenceTables.fun16 `shouldBe` Right StackTestRefrenceTables.fun17
	  it "executes SUM aggregate function" $ do
	    Lib2.executeStatement D.database StackTestRefrenceTables.fun18 `shouldBe` Right StackTestRefrenceTables.fun19
  describe "Lib3.runExecuteIO and Lib3.executeSql" $ do
    it "executes SELECT with several tables" $ do
      df <- runExecuteIO $ Lib3.executeSql "SELECT * FROM employees, flags;"
      df `shouldBe` Right selectLib3_1
    it "executes SELECT with several tables and named columns" $ do
      df <- runExecuteIO $ Lib3.executeSql "SELECT employees.id, flags.value FROM flags, employees;" 
      df `shouldBe` Right selectLib3_2
    it "executes the NOW() function" $ do
      df <- runExecuteIO $ Lib3.executeSql "SELECT now() FROM empty_table;" 
      df `shouldBe` Right selectLib3_3
    it "executes INSERT statement with the general table layout" $ do
      df <- runExecuteIO $ Lib3.executeSql "INSERT INTO employees VALUES (3, 'John', 'Doe');" 
      df `shouldBe` Right insertTable1
    it "executes INSERT statement with multiple VALUES" $ do
      df <-runExecuteIO $ Lib3.executeSql "INSERT INTO employees VALUES (4, 'Name', 'Surname'), (5, 'NoSurname', null), (6, 'R2D2'), (7);" 
      df `shouldBe` Right insertTable2
    it "executes INSERT statement with clarified rows to which you want to write" $ do
      df <- runExecuteIO $ Lib3.executeSql "INSERT INTO employees (id, surname) VALUES (8, 'Updating');" 
      df `shouldBe` Right insertTable3
    it "executes INSERT statement with MAX(), SUM() and NOW() functions" $ do
      df <- runExecuteIO $ Lib3.executeSql "INSERT INTO employees (surname, id, name) VALUES (NOW(), SUM(id), MAX(name));" 
      df `shouldBe` Right insertTable4
    it "executes UPDATE statement with clarified values for each column with a WHERE clause" $ do
      df <- runExecuteIO $ Lib3.executeSql "UPDATE employees SET name = 'Equal', surname = 'Two' WHERE id = 2;"
      df `shouldBe` Right updateTable1 
    it "executes UPDATE statement with MAX(), SUM(), NOW() functions" $ do
      df <- runExecuteIO $ Lib3.executeSql "UPDATE employees SET id = SUM(id), name = MAX(name), surname = NOW() WHERE id >= 2;" 
      df `shouldBe` Right updateTable2 
    it "executes DELETE statement with a WHERE clause" $ do
      df <- runExecuteIO $ Lib3.executeSql "DELETE FROM employees WHERE id = 1;"
      df `shouldBe` Right deleteTable1
    it "executed DELETE statement for all rows" $ do
      df <- runExecuteIO $ Lib3.executeSql "DELETE FROM employees;"
      df `shouldBe` Right deleteTable2
  describe "YamlHandler.toDFExpr" $ do
    it "works with StringValue" $ do
      YH.toDFExpr (DF.StringValue "value") `shouldBe` OValue (DF.StringValue "value")
    it "works with IntegerValue" $ do
      YH.toDFExpr (DF.IntegerValue 100) `shouldBe` OValue (DF.IntegerValue 100)
    it "works with BoolValue" $ do
      YH.toDFExpr (DF.BoolValue True) `shouldBe` OValue (DF.BoolValue True)
    it "works with NullValue" $ do
      YH.toDFExpr DF.NullValue `shouldBe` OValue DF.NullValue
    it "works with Column" $ do
      YH.toDFExpr (DF.Column "col1" DF.StringType) `shouldBe` YH.OColumn "col1" DF.StringType
    it "works with DataFrame" $ do
      YH.toDFExpr (snd D.tableEmployees) `shouldBe` employeesToDFExpr
  describe "YamlHandler.render" $ do
    it "renders tables with no rows" $ do
      YH.render (YH.OTable (YH.OColumns [YH.OColumn "name" DF.StringType]) (YH.ORows [])) `shouldBe` emptyTableYAML
    it "renders tables with values" $ do
      YH.render employeesToDFExpr `shouldBe` employeesYAML

emptyTableYAML :: String
emptyTableYAML = 
  concat 
    [ 
      "- - - name\n",
      "    - StringType\n",
      "- []\n"]

employeesYAML :: String
employeesYAML = 
  concat 
  [ 
    "- - - id\n",
    "    - IntegerType\n",
    "  - - name\n",
    "    - StringType\n",
    "  - - surname\n",
    "    - StringType\n",
    "- - - contents: 1\n",
    "      tag: IntegerValue\n",
    "    - contents: Vi\n",
    "      tag: StringValue\n",
    "    - contents: Po\n",
    "      tag: StringValue\n",
    "  - - contents: 2\n",
    "      tag: IntegerValue\n",
    "    - contents: Ed\n",
    "      tag: StringValue\n",
    "    - contents: Dl\n",
    "      tag: StringValue\n"] 


employeesToDFExpr :: YH.DFExpr
employeesToDFExpr = 
  YH.OTable 
  (YH.OColumns 
    [YH.OColumn "id" DF.IntegerType
    ,YH.OColumn "name" DF.StringType
    ,YH.OColumn "surname" DF.StringType]) 
  (YH.ORows 
    [YH.ORow 
      [YH.OValue (DF.IntegerValue 1)
      ,YH.OValue (DF.StringValue "Vi")
      ,YH.OValue (DF.StringValue "Po")]
    ,YH.ORow 
      [YH.OValue (DF.IntegerValue 2)
      ,YH.OValue (DF.StringValue "Ed")
      ,YH.OValue (DF.StringValue "Dl")]
      ])

selectLib3_1 :: DF.DataFrame 
selectLib3_1 = 
  DF.DataFrame 
  [ DF.Column "employees.id" DF.IntegerType,
    DF.Column "employees.name" DF.StringType,
    DF.Column "employees.surname" DF.StringType,
    DF.Column "flags.flag" DF.StringType,
    DF.Column "flags.value" DF.BoolType] 
  [ [DF.IntegerValue 1,DF.StringValue "Vi",DF.StringValue "Po",DF.StringValue "a",DF.BoolValue True],
    [DF.IntegerValue 1,DF.StringValue "Vi",DF.StringValue "Po",DF.StringValue "b",DF.BoolValue True],
    [DF.IntegerValue 1,DF.StringValue "Vi",DF.StringValue "Po",DF.StringValue "b",DF.NullValue],
    [DF.IntegerValue 1,DF.StringValue "Vi",DF.StringValue "Po",DF.StringValue "b",DF.BoolValue False],
    [DF.IntegerValue 2,DF.StringValue "Ed",DF.StringValue "Dl",DF.StringValue "a",DF.BoolValue True],
    [DF.IntegerValue 2,DF.StringValue "Ed",DF.StringValue "Dl",DF.StringValue "b",DF.BoolValue True],
    [DF.IntegerValue 2,DF.StringValue "Ed",DF.StringValue "Dl",DF.StringValue "b",DF.NullValue],
    [DF.IntegerValue 2,DF.StringValue "Ed",DF.StringValue "Dl",DF.StringValue "b",DF.BoolValue False]]

selectLib3_2 :: DF.DataFrame 
selectLib3_2 = 
  DF.DataFrame 
  [ DF.Column "employees.id" DF.IntegerType,
    DF.Column "flags.value" DF.BoolType] 
  [ [DF.IntegerValue 1,DF.BoolValue True],
    [DF.IntegerValue 2,DF.BoolValue True],
    [DF.IntegerValue 1,DF.BoolValue True],
    [DF.IntegerValue 2,DF.BoolValue True],
    [DF.IntegerValue 1,DF.NullValue],
    [DF.IntegerValue 2,DF.NullValue],
    [DF.IntegerValue 1,DF.BoolValue False],
    [DF.IntegerValue 2,DF.BoolValue False]] 

selectLib3_3 :: DF.DataFrame 
selectLib3_3 = 
  DF.DataFrame 
  [ DF.Column GC._TIME_COLUMN_NAME DF.StringType]
  [ [DF.StringValue "2023-12-12 00:37:56.740852 UTC"]]

insertTable1 :: DF.DataFrame 
insertTable1 = 
  DF.DataFrame 
  [ DF.Column "id" DF.IntegerType,
    DF.Column "name" DF.StringType,
    DF.Column "surname" DF.StringType] 
  [ [DF.IntegerValue 1,DF.StringValue "Vi",DF.StringValue "Po"],
    [DF.IntegerValue 2,DF.StringValue "Ed",DF.StringValue "Dl"],
    [DF.IntegerValue 3,DF.StringValue "John",DF.StringValue "Doe"]]

insertTable2 :: DF.DataFrame 
insertTable2 = 
  DF.DataFrame 
  [ DF.Column "id" DF.IntegerType,
    DF.Column "name" DF.StringType,
    DF.Column "surname" DF.StringType] 
  [ [DF.IntegerValue 1,DF.StringValue "Vi",DF.StringValue "Po"],
    [DF.IntegerValue 2,DF.StringValue "Ed",DF.StringValue "Dl"],
    [DF.IntegerValue 4,DF.StringValue "Name",DF.StringValue "Surname"],
    [DF.IntegerValue 5,DF.StringValue "NoSurname",DF.NullValue],
    [DF.IntegerValue 6,DF.StringValue "R2D2",DF.NullValue],
    [DF.IntegerValue 7,DF.NullValue,DF.NullValue]]

insertTable3 :: DF.DataFrame 
insertTable3 = 
  DF.DataFrame 
  [ DF.Column "id" DF.IntegerType,
    DF.Column "name" DF.StringType,
    DF.Column "surname" DF.StringType] 
  [ [DF.IntegerValue 1,DF.StringValue "Vi",DF.StringValue "Po"],
    [DF.IntegerValue 2,DF.StringValue "Ed",DF.StringValue "Dl"],
    [DF.IntegerValue 8,DF.NullValue,DF.StringValue "Updating"]]

insertTable4 :: DF.DataFrame 
insertTable4 = 
  DF.DataFrame 
  [ DF.Column "id" DF.IntegerType,
    DF.Column "name" DF.StringType,
    DF.Column "surname" DF.StringType] 
  [ [DF.IntegerValue 1,DF.StringValue "Vi",DF.StringValue "Po"],
    [DF.IntegerValue 2,DF.StringValue "Ed",DF.StringValue "Dl"],
    [DF.IntegerValue 3,DF.StringValue "Vi",DF.StringValue "2023-12-12 00:37:56.740852 UTC"]]

updateTable1 :: DF.DataFrame 
updateTable1 = 
  DF.DataFrame 
  [ DF.Column "id" DF.IntegerType,
    DF.Column "name" DF.StringType,
    DF.Column "surname" DF.StringType] 
  [ [DF.IntegerValue 1,DF.StringValue "Vi",DF.StringValue "Po"],
    [DF.IntegerValue 2,DF.StringValue "Equal",DF.StringValue "Two"]]

updateTable2 :: DF.DataFrame 
updateTable2 = 
  DF.DataFrame 
  [ DF.Column "id" DF.IntegerType,
    DF.Column "name" DF.StringType,
	DF.Column "surname" DF.StringType] 
  [ [DF.IntegerValue 1,DF.StringValue "Vi",DF.StringValue "Po"],
    [DF.IntegerValue 3,DF.StringValue "Vi",DF.StringValue "2023-12-12 00:37:56.740852 UTC"]]

deleteTable1 :: DF.DataFrame  
deleteTable1 =
  DF.DataFrame 
  [ DF.Column "id" DF.IntegerType,
    DF.Column "name" DF.StringType,
	DF.Column "surname" DF.StringType] 
  [ [DF.IntegerValue 2,DF.StringValue "Ed",DF.StringValue "Dl"]]

deleteTable2 :: DF.DataFrame 
deleteTable2 =
  DF.DataFrame 
  [ DF.Column "id" DF.IntegerType,
    DF.Column "name" DF.StringType,
	DF.Column "surname" DF.StringType] 
  []
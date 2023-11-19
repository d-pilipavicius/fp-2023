import Data.Either
import Data.Maybe ()
import InMemoryTables qualified as D
import Lib1
import Lib2
import StackTestRefrenceTables
import Test.Hspec

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
	  Lib2.executeStatement StackTestRefrenceTables.fun2 `shouldSatisfy` isLeft
	it "recognises only case sensitive table names" $ do
	  Lib2.executeStatement StackTestRefrenceTables.fun20 `shouldSatisfy` isLeft
	it "recognises only existing columns in tables" $ do
	  Lib2.executeStatement StackTestRefrenceTables.fun8 `shouldSatisfy` isLeft
	it "recognises only case sensitive column names" $ do
	  Lib2.executeStatement StackTestRefrenceTables.fun21 `shouldSatisfy` isLeft
	it "recognises non IntegerType provided columns" $ do
	  Lib2.executeStatement StackTestRefrenceTables.fun10 `shouldSatisfy` isLeft
	it "recognises only existing operators" $ do
	  Lib2.executeStatement StackTestRefrenceTables.fun11 `shouldSatisfy` isLeft
	it "executes correct select statements" $ do
	  Lib2.executeStatement StackTestRefrenceTables.fun1 `shouldBe` Right StackTestRefrenceTables.fun9
	it "executes selected columns" $ do
	  Lib2.executeStatement StackTestRefrenceTables.fun12 `shouldBe` Right StackTestRefrenceTables.fun13
	it "executes select statements with WHERE, OR clauses" $ do
	  Lib2.executeStatement StackTestRefrenceTables.fun14 `shouldBe` Right StackTestRefrenceTables.fun15
	it "executes MAX aggregate function" $ do
	  Lib2.executeStatement StackTestRefrenceTables.fun16 `shouldBe` Right StackTestRefrenceTables.fun17
	it "executes SUM aggregate function" $ do
	  Lib2.executeStatement StackTestRefrenceTables.fun18 `shouldBe` Right StackTestRefrenceTables.fun19

 
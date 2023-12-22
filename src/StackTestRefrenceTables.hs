{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module StackTestRefrenceTables where

import DataFrame
import CustomDataTypes

fun1 :: ParsedStatement
fun1 = ParsedTable {table = "employees", columns = ColumnList [AllC], rowConditions = NoConditions}

fun2 :: ParsedStatement
fun2 = ParsedTable {table = "table", columns = ColumnList [AllC], rowConditions = Conditions [("col1",">","col2")]}

fun3 :: ParsedStatement
fun3 = ParsedTable {table = "table", columns = ColumnList [AllC], rowConditions = Conditions [("col1",">","col2"),("col2","<=","col3"),("2",">","1")]}

fun4 :: ParsedStatement
fun4 = ParsedTable {table = "employees", columns = ColumnList [OneColumn "id"], rowConditions = NoConditions}

fun5 :: ParsedStatement
fun5 = ParsedTables {tables = AllT}

fun6 :: ParsedStatement
fun6 = ParsedTables {tables = Table "employees"}

fun7 :: ParsedStatement
fun7 = ParsedTable {table = "employees", columns = ColumnList [Func {name = "max", column = OneColumn "id"}], rowConditions = NoConditions}

fun8 :: ParsedStatement
fun8 = ParsedTable {table = "employees", columns = ColumnList [OneColumn "veryNiceValues"], rowConditions = NoConditions}

fun9 :: DataFrame
fun9 = DataFrame [Column "id" IntegerType,Column "name" StringType,Column "surname" StringType] [[IntegerValue 1,StringValue "Vi",StringValue "Po"],[IntegerValue 2,StringValue "Ed",StringValue "Dl"]] 

fun10 :: ParsedStatement
fun10 = ParsedTable {table = "employees", columns = Func {name = "max", column = OneColumn "id"}, rowConditions = Conditions [("name",">","10")]}

fun11 :: ParsedStatement
fun11 = ParsedTable {table = "employees", columns = ColumnList [AllC], rowConditions = Conditions [("id","><","10")]}

fun12 :: ParsedStatement
fun12 = ParsedTable {table = "employees", columns = ColumnList [OneColumn "id", OneColumn"name"], rowConditions = NoConditions}

fun13 :: DataFrame
fun13 = DataFrame [Column "id" IntegerType,Column "name" StringType] [[IntegerValue 1,StringValue "Vi"],[IntegerValue 2,StringValue "Ed"]]

fun14 :: ParsedStatement 
fun14 = ParsedTable {table = "employees", columns = AllC, rowConditions = Conditions [("id","<","1"),("id","=","1")]}

fun15 :: DataFrame
fun15 = DataFrame [Column "id" IntegerType,Column "name" StringType,Column "surname" StringType] [[IntegerValue 1,StringValue "Vi",StringValue "Po"]]

fun16 :: ParsedStatement
fun16 = ParsedTable {table = "employees", columns = Func {name = "max", column = OneColumn "id"}, rowConditions = NoConditions}

fun17 :: DataFrame
fun17 = DataFrame [Column "max_id" IntegerType] [[IntegerValue 2]]

fun18 :: ParsedStatement
fun18 = ParsedTable {table = "employees", columns = Func {name = "sum", column = OneColumn "id"}, rowConditions = NoConditions}

fun19 :: DataFrame
fun19 = DataFrame [Column "sum_id" IntegerType] [[IntegerValue 3]]

fun20 :: ParsedStatement
fun20 = ParsedTable {table = "employeEs", columns = OneColumn "id", rowConditions = NoConditions}

fun21 :: ParsedStatement
fun21 = ParsedTable {table = "employees", columns = ColumnList [OneColumn "Id"], rowConditions = NoConditions}
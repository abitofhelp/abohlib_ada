--   =============================================================================
--   Test_Type_Safe_Generic_Id - Implementation
--   =============================================================================
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;
pragma Warnings (Off, "subprogram body has no previous spec");

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id;

package body Test_Type_Safe_Generic_Id is

--   ==========================================================================
--   Test ID Types Setup
--   ==========================================================================

--  Define different category types for testing
   type User_Category is null record;
   type Product_Category is null record;
   type Order_Category is null record;

--  Create ID packages for each category
   package User_Id_Pkg is new
      Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id.Generic_ID_Type
        (Category      => User_Category,
         Category_Name => "User",
         Prefix        => "USR");

   package Product_Id_Pkg is new
      Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id.Generic_ID_Type
        (Category      => Product_Category,
         Category_Name => "Product",
         Prefix        => "PRD");

   package Order_Id_Pkg is new
      Abohlib.Core.Domain.Value_Objects.Type_Safe_Generic_Id.Generic_ID_Type
        (Category      => Order_Category,
         Category_Name => "Order",
         Prefix        => "ORD");

--  Convenient subtypes
   subtype User_Id is User_Id_Pkg.ID;
   subtype Product_Id is Product_Id_Pkg.ID;
   subtype Order_Id is Order_Id_Pkg.ID;

--  Use clauses for operators
   use User_Id_Pkg;
   use Product_Id_Pkg;
   use Order_Id_Pkg;

--   ==========================================================================
--   Test Functions
--   ==========================================================================

   function Test_Create_Id return Void_Result.Result is
      Id1 : constant User_Id := User_Id_Pkg.New_ID;
      Id2 : constant User_Id := User_Id_Pkg.New_ID;
   begin
--  Verify IDs are not null
      if User_Id_Pkg.Is_Null (Id1) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Created ID should not be null"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Id")
         ));
      end if;

--  Verify IDs are different
      if Id1 = Id2 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Two created IDs should be different"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Create_Id")
         ));
      end if;

--  Verify prefix
      declare
         Id_String : constant String := User_Id_Pkg.To_String (Id1);
      begin
         if Id_String'Length < 3 or else Id_String (1 .. 3) /= "USR" then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("ID should start with correct prefix"),
               Details     => To_Unbounded_String ("Got: " & Id_String),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Create_Id")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Create_Id;

   function Test_From_String return Void_Result.Result is
--  Create a valid ID string
      Original_Id : constant Product_Id := Product_Id_Pkg.New_ID;
      Id_String : constant String := Product_Id_Pkg.To_String (Original_Id);
      Parsed_Id : Product_Id;
   begin
--  Parse the ID string
      declare
         Parse_Result : constant Product_Id_Pkg.Result_Type := Product_Id_Pkg.From_String (Id_String);
      begin
         if not Product_Id_Pkg.Is_Ok (Parse_Result) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Failed to parse valid ID string"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_From_String")
            ));
         end if;
         Parsed_Id := Product_Id_Pkg.Get_Ok (Parse_Result);
      end;

--  Verify they are equal
      if Parsed_Id /= Original_Id then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Parsed ID should equal original"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_From_String")
         ));
      end if;

--  Test with a known valid ULID string
      declare
         Test_Result : constant Product_Id_Pkg.Result_Type :=
            Product_Id_Pkg.From_String ("PRD01HQ3PJKXG2Q8S7F5XRJM9Z3AB");
         Test_Id : Product_Id;
      begin
         if not Product_Id_Pkg.Is_Ok (Test_Result) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Valid ID string should parse successfully"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_From_String")
            ));
         end if;

         Test_Id := Product_Id_Pkg.Get_Ok (Test_Result);
         if Product_Id_Pkg.Is_Null (Test_Id) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Valid ID string should parse"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_From_String")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_From_String;

   function Test_Invalid_Id_String return Void_Result.Result is
   begin
--  Test empty string
      declare
         Result : constant Order_Id_Pkg.Result_Type := Order_Id_Pkg.From_String ("");
      begin
         if Order_Id_Pkg.Is_Ok (Result) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Empty string should return error"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Invalid_Id_String")
            ));
         end if;
      end;

--  Test wrong prefix
      declare
         Result : constant Order_Id_Pkg.Result_Type := Order_Id_Pkg.From_String ("USR-01HQ3PJKXG2Q8S7F5XRJM9Z3AB");
      begin
         if Order_Id_Pkg.Is_Ok (Result) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Wrong prefix should return error"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Invalid_Id_String")
            ));
         end if;
      end;

--  Test invalid ULID format
      declare
         Result : constant Order_Id_Pkg.Result_Type := Order_Id_Pkg.From_String ("ORD-INVALID_ULID_STRING");
      begin
         if Order_Id_Pkg.Is_Ok (Result) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Invalid ULID should return error"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Invalid_Id_String")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Invalid_Id_String;

   function Test_To_String return Void_Result.Result is
      Id : constant User_Id := User_Id_Pkg.New_ID;
      Id_String : constant String := User_Id_Pkg.To_String (Id);
   begin
--  Check format: PREFIX+ULID
      if Id_String'Length /= 29 then -- 3 (prefix) + 26 (ULID)
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("ID string should be 29 characters"),
            Details     => To_Unbounded_String ("Length: " & Id_String'Length'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_To_String")
         ));
      end if;

--  Check prefix
      if Id_String (1 .. 3) /= "USR" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("ID string should start with USR"),
            Details     => To_Unbounded_String ("Got: " & Id_String (1 .. 3)),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_To_String")
         ));
      end if;

--  Check ULID part contains only valid characters
      for I in 4 .. Id_String'Last loop
         case Id_String (I) is
            when '0' .. '9' | 'A' .. 'Z' =>
               null; -- Valid
            when others =>
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Invalid character in ULID"),
                  Details     => To_Unbounded_String ("Char: " & Id_String (I)),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_To_String")
               ));
         end case;
      end loop;

      return Void_Result.Ok (True);
   end Test_To_String;

   function Test_Id_Equality return Void_Result.Result is
      Id1 : constant Product_Id := Product_Id_Pkg.New_ID;
      Id2 : constant Product_Id := Product_Id_Pkg.New_ID;
      Id_String : constant String := Product_Id_Pkg.To_String (Id1);
      Id1_Copy : Product_Id;
   begin
--  Parse the string to get copy
      declare
         Parse_Result : constant Product_Id_Pkg.Result_Type := Product_Id_Pkg.From_String (Id_String);
      begin
         if not Product_Id_Pkg.Is_Ok (Parse_Result) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Failed to parse valid ID string for copy"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Id_Equality")
            ));
         end if;
         Id1_Copy := Product_Id_Pkg.Get_Ok (Parse_Result);
      end;

--  Test equality of same ID
      if Id1 /= Id1_Copy then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Same IDs should be equal"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Id_Equality")
         ));
      end if;

--  Test inequality of different IDs
      if Id1 = Id2 then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Different IDs should not be equal"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Id_Equality")
         ));
      end if;

      return Void_Result.Ok (True);
   end Test_Id_Equality;

   function Test_Id_Ordering return Void_Result.Result is
--  Create IDs with small delay to ensure different timestamps
      Id1 : constant Order_Id := Order_Id_Pkg.New_ID;
   begin
      delay 0.001; -- 1 millisecond delay
      declare
         Id2 : constant Order_Id := Order_Id_Pkg.New_ID;
      begin
         delay 0.001; -- 1 millisecond delay
         declare
            Id3 : constant Order_Id := Order_Id_Pkg.New_ID;
         begin
--  ULIDs are lexicographically sortable by time
--  Later IDs should be "greater" than earlier ones
            if not (Id1 < Id2) then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Earlier ID should be less than later ID"),
                  Details     => Null_Unbounded_String,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Id_Ordering")
               ));
            end if;

            if not (Id2 < Id3) then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Ordering should be transitive"),
                  Details     => Null_Unbounded_String,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Id_Ordering")
               ));
            end if;

--  Test ordering operators
            if Id1 >= Id2 or Id2 >= Id3 then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String (">= operator should work correctly"),
                  Details     => Null_Unbounded_String,
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Id_Ordering")
               ));
            end if;

            return Void_Result.Ok (True);
         end; -- Id3 declare block
      end; -- Id2 declare block
   end Test_Id_Ordering;

   function Test_Prefix_Validation return Void_Result.Result is
--  Test that prefixes are type-safe
      User_ID_Val : constant User_Id := User_Id_Pkg.New_ID;
      User_String : constant String := User_Id_Pkg.To_String (User_ID_Val);
      Product_ID_Val : constant Product_Id := Product_Id_Pkg.New_ID;
      Product_String : constant String := Product_Id_Pkg.To_String (Product_ID_Val);
   begin
--  Verify prefixes
      if User_String (1 .. 3) /= "USR" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("User ID should have USR prefix"),
            Details     => To_Unbounded_String ("Got: " & User_String (1 .. 3)),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Prefix_Validation")
         ));
      end if;

      if Product_String (1 .. 3) /= "PRD" then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Product ID should have PRD prefix"),
            Details     => To_Unbounded_String ("Got: " & Product_String (1 .. 3)),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Prefix_Validation")
         ));
      end if;

--  Types should be incompatible (compile-time check)
--  The following would not compile:
--  User_ID := Product_ID;  -- Type error!

      return Void_Result.Ok (True);
   end Test_Prefix_Validation;

   function Test_Id_Uniqueness return Void_Result.Result is
--  Generate many IDs and check uniqueness
      type Id_Array is array (1 .. 100) of User_Id;
      Ids : Id_Array;
   begin
--  Generate IDs
      for I in Ids'Range loop
         Ids (I) := User_Id_Pkg.New_ID;
      end loop;

--  Check all are unique
      for I in Ids'Range loop
         for J in Ids'Range loop
            if I /= J and then Ids (I) = Ids (J) then
               return Void_Result.Err (Test_Error'(
                  Kind        => Assertion_Failed,
                  Message     => To_Unbounded_String ("Generated duplicate ID"),
                  Details     => To_Unbounded_String ("Indices: " & I'Image & " and " & J'Image),
                  Line_Number => 0,
                  Test_Name   => To_Unbounded_String ("Test_Id_Uniqueness")
               ));
            end if;
         end loop;
      end loop;

      return Void_Result.Ok (True);
   end Test_Id_Uniqueness;

   function Test_Null_Id return Void_Result.Result is
      Null_Id : constant Order_Id := Order_Id_Pkg.Null_ID;
      Valid_Id : constant Order_Id := Order_Id_Pkg.New_ID;
   begin
--  Test Is_Null
      if not Order_Id_Pkg.Is_Null (Null_Id) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Null_ID should be null"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Null_Id")
         ));
      end if;

      if Order_Id_Pkg.Is_Null (Valid_Id) then
         return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Created ID should not be null"),
            Details     => Null_Unbounded_String,
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Null_Id")
         ));
      end if;

--  Test null ID string representation
      declare
         Null_String : constant String := Order_Id_Pkg.To_String (Null_Id);
      begin
         if Null_String'Length /= 29 then  -- 3 (prefix) + 26 (ULID)
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Null ID string should be 29 characters"),
               Details     => To_Unbounded_String ("Got: " & Null_String),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Null_Id")
            ));
         end if;

         if Null_String (1 .. 3) /= "ORD" then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Null ID string should start with ORD"),
               Details     => To_Unbounded_String ("Got: " & Null_String (1 .. 3)),
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Null_Id")
            ));
         end if;
      end;

      return Void_Result.Ok (True);
   end Test_Null_Id;

   function Test_Id_Hash return Void_Result.Result is
      Id1 : constant Product_Id := Product_Id_Pkg.New_ID;
      Id2 : constant Product_Id := Product_Id_Pkg.New_ID;
      Id_String : constant String := Product_Id_Pkg.To_String (Id1);
      Id1_Copy : Product_Id;
   begin
--  Parse the string to get copy
      declare
         Parse_Result : constant Product_Id_Pkg.Result_Type := Product_Id_Pkg.From_String (Id_String);
      begin
         if not Product_Id_Pkg.Is_Ok (Parse_Result) then
            return Void_Result.Err (Test_Error'(
               Kind        => Assertion_Failed,
               Message     => To_Unbounded_String ("Failed to parse valid ID string for hash test"),
               Details     => Null_Unbounded_String,
               Line_Number => 0,
               Test_Name   => To_Unbounded_String ("Test_Id_Hash")
            ));
         end if;
         Id1_Copy := Product_Id_Pkg.Get_Ok (Parse_Result);
      end;

--  Compute hashes
      declare
         Hash1 : constant Ada.Containers.Hash_Type := Product_Id_Pkg.Hash (Id1);
         Hash2 : constant Ada.Containers.Hash_Type := Product_Id_Pkg.Hash (Id2);
         Hash1_Copy : constant Ada.Containers.Hash_Type := Product_Id_Pkg.Hash (Id1_Copy);
      begin
         --  Same IDs should have same hash
         if Hash1 /= Hash1_Copy then
            return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Same IDs should have same hash"),
            Details     => To_Unbounded_String ("Hash1: " & Hash1'Image &
                                              ", Hash1_Copy: " & Hash1_Copy'Image),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Id_Hash")
         ));
         end if;

         --  Different IDs should (probably) have different hashes
         --  This could fail by chance, but very unlikely
         if Hash1 = Hash2 then
            return Void_Result.Err (Test_Error'(
            Kind        => Assertion_Failed,
            Message     => To_Unbounded_String ("Different IDs should have different hashes"),
            Details     => To_Unbounded_String ("Hash collision detected"),
            Line_Number => 0,
            Test_Name   => To_Unbounded_String ("Test_Id_Hash")
         ));
         end if;

         return Void_Result.Ok (True);
      end; -- End of declare block for hashes
   end Test_Id_Hash;

--   ==========================================================================
--   Run All Tests
--   ==========================================================================

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   is
      Tests : Test_Results_Array (1 .. 10);
      Index : Positive := 1;

      procedure Add_Test_Result
        (Name : String;
         Test_Func : Test_Function_Access)
      is
         Result : constant Test_Result_Pkg.Result :=
            Run_Test (Name, Test_Func, Output);
      begin
         if Result.Is_Ok then
            Tests (Index) := Result.Get_Ok;
            Print_Test_Result (Tests (Index), Output);
            Index := Index + 1;
         else
--  Handle test execution error
            declare
               Error : constant Test_Error := Result.Get_Err;
            begin
               Tests (Index) := Test_Result'(
                  Name           => To_Unbounded_String (Name),
                  Status         => Failed,
                  Message        => Error.Message,
                  Elapsed_Time   => 0.0,
                  Line_Number    => Error.Line_Number,
                  Correlation_ID => To_Unbounded_String ("TEST-" & Name)
               );
               Print_Test_Result (Tests (Index), Output);
               Index := Index + 1;
            end;
         end if;
      end Add_Test_Result;

   begin
      Output.Write_Line ("=== Running Type_Safe_Generic_Id Unit Tests ===");
      Output.Write_Line ("");

--  Run all tests
      Add_Test_Result ("Test_Create_Id", Test_Create_Id'Access);
      Add_Test_Result ("Test_From_String", Test_From_String'Access);
      Add_Test_Result ("Test_Invalid_Id_String", Test_Invalid_Id_String'Access);
      Add_Test_Result ("Test_To_String", Test_To_String'Access);
      Add_Test_Result ("Test_Id_Equality", Test_Id_Equality'Access);
      Add_Test_Result ("Test_Id_Ordering", Test_Id_Ordering'Access);
      Add_Test_Result ("Test_Prefix_Validation", Test_Prefix_Validation'Access);
      Add_Test_Result ("Test_Id_Uniqueness", Test_Id_Uniqueness'Access);
      Add_Test_Result ("Test_Null_Id", Test_Null_Id'Access);
      Add_Test_Result ("Test_Id_Hash", Test_Id_Hash'Access);

--  Generate summary
      declare
         Stats_Result : constant Test_Stats_Result.Result :=
            Run_Test_Suite ("Type_Safe_Generic_Id_Tests", Tests (1 .. Index - 1), Output);
      begin
         if Stats_Result.Is_Ok then
            declare
               Stats : constant Test_Statistics := Stats_Result.Get_Ok;
            begin
               Output.Write_Line ("");
               Print_Test_Summary ("Type_Safe_Generic_Id Unit Tests", Stats, Output);
               return Test_Stats_Result.Ok (Stats);
            end;
         else
            return Stats_Result;
         end if;
      end;
   end Run_All_Tests;

end Test_Type_Safe_Generic_Id;

pragma Warnings (On, "subprogram body has no previous spec");

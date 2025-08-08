--  =============================================================================
--  Abohlib.Core.Domain.Result.Functional - Functional Operations for Result
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    This package provides functional programming operations for Result types.
--    These operations allow you to chain computations that might fail, without
--    writing nested if-then-else statements.
--
--  Key Concepts:
--    - Map: Transform successful values while preserving errors
--    - Map_Err: Transform error values while preserving successes
--    - And_Then: Chain operations that return Results (also called flatMap/bind)
--    - Or_Else: Provide alternative Results when errors occur
--
--  Why Generic Functions?
--    Ada requires these operations to be generic functions rather than methods
--    because Ada doesn't allow nested generic instantiations within generic
--    packages. Each operation must be instantiated for your specific Result types.
--
--  Example Usage:
--    ```ada
--    -- First, instantiate the Result package for your types
--    package String_Result is new Result_Package (String, Error_Type);
--    package Integer_Result is new Result_Package (Integer, Error_Type);
--    
--    -- Instantiate the Map function to transform String to Integer
--    function String_To_Int is new Map
--      (Source_Result => String_Result,
--       Target_Result => Integer_Result,
--       Transform => Integer'Value);
--    
--    -- Use it to transform a Result
--    Str_Result : String_Result.Result := String_Result.Ok ("42");
--    Int_Result : Integer_Result.Result := String_To_Int (Str_Result);
--    ```
--
--  Common Patterns:
--    1. Error Propagation: Use And_Then to chain operations that might fail
--    2. Error Recovery: Use Or_Else to provide fallback values
--    3. Value Transformation: Use Map to transform successful values
--    4. Error Enhancement: Use Map_Err to add context to errors
--  =============================================================================

pragma Ada_2022;

package Abohlib.Core.Domain.Result.Functional is
   pragma Preelaborate;

   --  ==========================================================================
   --  Map Operation
   --  ==========================================================================
   --  Transforms successful (Ok) values while preserving errors unchanged.
   --  
   --  What it does:
   --    - If the Result is Ok: applies Transform function to the value
   --    - If the Result is Err: passes the error through unchanged
   --
   --  When to use:
   --    Use Map when you need to transform a successful value but want errors
   --    to automatically propagate without handling them explicitly.
   --
   --  Example:
   --    -- Convert a Result containing a String to a Result containing its length
   --    function Get_Length is new Map
   --      (Source_Result => String_Result,
   --       Target_Result => Natural_Result,
   --       Transform => Ada.Strings.Unbounded.Length);
   --
   --  Note: Both Results must have the same error type

   generic
      with package Source_Result is new Result_Package (<>);
      with package Target_Result is new Result_Package 
         (Ok_Type => <>, Err_Type => Source_Result.Err_Type);
      with function Transform (Value : Source_Result.Ok_Type) 
         return Target_Result.Ok_Type;
   function Map (R : Source_Result.Result) return Target_Result.Result
   with
      Post =>
         (if Source_Result.Is_Ok (R) then
            Target_Result.Is_Ok (Map'Result)
          else
            Target_Result.Is_Err (Map'Result));

   --  ==========================================================================
   --  Map_Err Operation
   --  ==========================================================================
   --  Transforms error values while preserving successful (Ok) values unchanged.
   --
   --  What it does:
   --    - If the Result is Ok: passes the value through unchanged
   --    - If the Result is Err: applies Transform function to the error
   --
   --  When to use:
   --    Use Map_Err when you need to transform or enhance error information,
   --    such as adding context or converting between error types.
   --
   --  Example:
   --    -- Add context to an error
   --    function Add_Context is new Map_Err
   --      (Source_Result => File_Result,
   --       Target_Result => Enhanced_Result,
   --       Transform => Wrap_With_Context);
   --
   --  Note: Both Results must have the same Ok type

   generic
      with package Source_Result is new Result_Package (<>);
      with package Target_Result is new Result_Package 
         (Ok_Type => Source_Result.Ok_Type, Err_Type => <>);
      with function Transform (Error : Source_Result.Err_Type)
         return Target_Result.Err_Type;
   function Map_Err (R : Source_Result.Result) return Target_Result.Result
   with
      Post =>
         (if Source_Result.Is_Err (R) then
            Target_Result.Is_Err (Map_Err'Result)
          else
            Target_Result.Is_Ok (Map_Err'Result));

   --  ==========================================================================
   --  And_Then Operation (Flat Map / Bind)
   --  ==========================================================================
   --  Chains operations that return Results, stopping at the first error.
   --
   --  What it does:
   --    - If the Result is Ok: applies Operation function (which returns a Result)
   --    - If the Result is Err: passes the error through without calling Operation
   --
   --  When to use:
   --    Use And_Then to chain multiple operations that might fail, where each
   --    operation depends on the success of the previous one. This avoids
   --    deeply nested if-statements.
   --
   --  Example:
   --    -- Chain file operations that might fail
   --    function Process_File is new And_Then
   --      (Source_Result => File_Result,
   --       Target_Result => Data_Result,
   --       Operation => Read_And_Parse);
   --    
   --    -- Usage: Open_File (Path) |> Process_File |> Save_Results
   --
   --  Note: This is also known as "flatMap" or "bind" in other languages.
   --        Both Results must have the same error type

   generic
      with package Source_Result is new Result_Package (<>);
      with package Target_Result is new Result_Package 
         (Ok_Type => <>, Err_Type => Source_Result.Err_Type);
      with function Operation (Value : Source_Result.Ok_Type)
         return Target_Result.Result;
   function And_Then (R : Source_Result.Result) return Target_Result.Result
   with
      Post =>
         (if Source_Result.Is_Err (R) then
            Target_Result.Is_Err (And_Then'Result));

   --  ==========================================================================
   --  Or_Else Operation
   --  ==========================================================================
   --  Provides an alternative Result when the original Result is an error.
   --
   --  What it does:
   --    - If the Result is Ok: returns the original Result unchanged
   --    - If the Result is Err: calls Alternative function to get a new Result
   --
   --  When to use:
   --    Use Or_Else to provide fallback values or alternative computations
   --    when an operation fails. This is useful for error recovery.
   --
   --  Example:
   --    -- Try to load from cache, fall back to database
   --    function Load_From_DB is new Or_Else
   --      (Result_Pkg => Data_Result,
   --       Alternative => Query_Database);
   --    
   --    -- Usage: Load_From_Cache (Key) |> Load_From_DB

   generic
      with package Result_Pkg is new Result_Package (<>);
      with function Alternative (Error : Result_Pkg.Err_Type)
         return Result_Pkg.Result;
   function Or_Else (R : Result_Pkg.Result) return Result_Pkg.Result;

   --  ==========================================================================
   --  Map with Same Types (Convenience)
   --  ==========================================================================
   --  Simplified Map operation for transforming values within the same Result type.
   --
   --  What it does:
   --    Same as Map, but both input and output have the same Ok and Err types.
   --
   --  When to use:
   --    Use Map_Same when transforming a value without changing its type,
   --    such as normalizing data or applying business logic.
   --
   --  Example:
   --    -- Normalize a string within the same Result type
   --    function Normalize is new Map_Same
   --      (Result_Pkg => String_Result,
   --       Transform => To_Upper);

   generic
      with package Result_Pkg is new Result_Package (<>);
      with function Transform (Value : Result_Pkg.Ok_Type)
         return Result_Pkg.Ok_Type;
   function Map_Same (R : Result_Pkg.Result) return Result_Pkg.Result
   with
      Post =>
         (if Result_Pkg.Is_Ok (R) then
            Result_Pkg.Is_Ok (Map_Same'Result)
          else
            Result_Pkg.Is_Err (Map_Same'Result));

   --  ==========================================================================
   --  Map_Err with Same Types (Convenience)
   --  ==========================================================================
   --  Map_Err operation where Ok and Err types remain the same

   generic
      with package Result_Pkg is new Result_Package (<>);
      with function Transform (Error : Result_Pkg.Err_Type)
         return Result_Pkg.Err_Type;
   function Map_Err_Same (R : Result_Pkg.Result) return Result_Pkg.Result
   with
      Post =>
         (if Result_Pkg.Is_Err (R) then
            Result_Pkg.Is_Err (Map_Err_Same'Result)
          else
            Result_Pkg.Is_Ok (Map_Err_Same'Result));

   --  ==========================================================================
   --  Additional Utility Operations
   --  ==========================================================================

   --  Unwrap_Or: Extract Ok value or return a default value
   --  
   --  What it does:
   --    - If the Result is Ok: returns the contained value
   --    - If the Result is Err: returns the provided default value
   --
   --  Example:
   --    Count : Natural := Unwrap_Or (Parse_Count (Input), Default => 0);
   generic
      with package Result_Pkg is new Result_Package (<>);
   function Unwrap_Or (R : Result_Pkg.Result; Default : Result_Pkg.Ok_Type)
      return Result_Pkg.Ok_Type;

   --  Unwrap_Or_Else: Extract Ok value or compute a default value
   --
   --  What it does:
   --    - If the Result is Ok: returns the contained value
   --    - If the Result is Err: calls Default_Fn with the error to compute a value
   --
   --  Example:
   --    Count : Natural := Unwrap_Or_Else (Parse_Count (Input), 
   --                                       Log_And_Return_Zero'Access);
   generic
      with package Result_Pkg is new Result_Package (<>);
      with function Default_Fn (Error : Result_Pkg.Err_Type)
         return Result_Pkg.Ok_Type;
   function Unwrap_Or_Else (R : Result_Pkg.Result) return Result_Pkg.Ok_Type;

   --  Is_Ok_And: Check if Result is Ok AND a condition is true
   --
   --  What it does:
   --    Returns True only if the Result is Ok AND the predicate returns True
   --    for the contained value. Returns False for Err or if predicate is False.
   --
   --  Example:
   --    if Is_Ok_And (Age_Result, Is_Adult'Access) then
   --       Grant_Access;
   generic
      with package Result_Pkg is new Result_Package (<>);
      with function Predicate (Value : Result_Pkg.Ok_Type) return Boolean;
   function Is_Ok_And (R : Result_Pkg.Result) return Boolean;

   --  Is_Err_And: Check if Result is Err AND a condition is true
   --
   --  What it does:
   --    Returns True only if the Result is Err AND the predicate returns True
   --    for the contained error. Returns False for Ok or if predicate is False.
   --
   --  Example:
   --    if Is_Err_And (File_Result, Is_Permission_Error'Access) then
   --       Request_Admin_Rights;
   generic
      with package Result_Pkg is new Result_Package (<>);
      with function Predicate (Error : Result_Pkg.Err_Type) return Boolean;
   function Is_Err_And (R : Result_Pkg.Result) return Boolean;

end Abohlib.Core.Domain.Result.Functional;
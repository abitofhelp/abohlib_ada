--  =============================================================================
--  Abohlib.Core.Domain.Repositories.Generic_Repository - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Abohlib.Core.Domain.Repositories.Generic_Repository is

   ---------------------------------
   -- Find_By_Specification --
   ---------------------------------

   function Find_By_Specification
     (Self : Repository_Interface'Class) return Find_All_Result.Result
   is

      All_Result : constant Find_All_Result.Result := Self.Find_All;
      Filtered   : Entity_Vectors.Vector;
   begin
      if All_Result.Is_Err then
         return All_Result;
      end if;

      declare
         All_Entities : constant Entity_Vectors.Vector := All_Result.Get_Ok;
      begin
         --  Parallel filtering using Ada 2022 features
         for Entity of All_Entities loop
            if Matches (Entity) then
               Filtered.Append (Entity);
            end if;
         end loop;

         return Find_All_Result.Ok (Filtered);
      end;
   end Find_By_Specification;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
      pragma Unreferenced (Position);
   begin
      return False;  --  Placeholder implementation
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Parallel_Iterator; Position : Cursor) return Entity_Type
   is
      pragma Unreferenced (Container, Position);
      Dummy : Entity_Type;
   begin
      return Dummy;  --  Placeholder implementation
   end Element;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Container : Parallel_Iterator) return Iterator_Interface'Class
   is
      pragma Unreferenced (Container);
   begin
      return
        Iterator_Interface'
          (Iterator_Interfaces.Forward_Iterator with null record);
   end Iterate;

   ------------------------------------
   -- Tampering_With_Cursors_Prohibited --
   ------------------------------------

   function Tampering_With_Cursors_Prohibited
     (Container : Parallel_Iterator) return Boolean
   is
      pragma Unreferenced (Container);
   begin
      return True;  --  Always safe in this implementation
   end Tampering_With_Cursors_Prohibited;

   ----------------------------
   -- Execute_In_Transaction --
   ----------------------------

   procedure Execute_In_Transaction
     (Self : Repository_Interface'Class; Operation : not null access procedure)
   is
      pragma Unreferenced (Self);

      Transaction : Transaction_Type;
   begin
      Begin_Transaction (Transaction);

      begin
         Operation.all;
         Commit (Transaction);
      exception
         when others =>
            Rollback (Transaction);
            raise;
      end;
   end Execute_In_Transaction;

   -----------
   -- First --
   -----------

   overriding
   function First (Object : Iterator_Interface) return Cursor is
      pragma Unreferenced (Object);
   begin
      return No_Element;
   end First;

   ----------
   -- Next --
   ----------

   overriding
   function Next (Object : Iterator_Interface; Position : Cursor) return Cursor
   is
      pragma Unreferenced (Object, Position);
   begin
      return No_Element;
   end Next;

   function Image (C : Cursor) return String is
   begin
      pragma Unreferenced (C);
      return "[Cursor]";
   end Image;

end Abohlib.Core.Domain.Repositories.Generic_Repository;

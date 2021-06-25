with Ada.Unchecked_Deallocation;

package body Optional.Values is

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (This : in out Optional) is
   begin
      if This.Element /= null then
         This.Element := new Element_Type'(This.Element.all);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Optional) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Element_Type, Element_Access);
   begin
      Free (This.Element);
   end Finalize;

   --------------
   -- Or_Raise --
   --------------

   function Or_Raise (This   : Optional;
                      Ex_Id  : Ada.Exceptions.Exception_Id;
                      Ex_Msg : String := "")
                      return Element_Type
   is
   begin
      if This.Has_Element then
         return This.Element.all;
      else
         Ada.Exceptions.Raise_Exception (Ex_Id, Ex_Msg);
      end if;
   end Or_Raise;

end Optional.Values;

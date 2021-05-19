package body Optional.Values is

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
         return This.Element.Constant_Reference;
      else
         Ada.Exceptions.Raise_Exception (Ex_Id, Ex_Msg);
      end if;
   end Or_Raise;

end Optional.Values;

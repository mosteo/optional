private with Ada.Containers.Indefinite_Holders;
with Ada.Exceptions;

generic
   type Element_Type (<>) is private;
   with function Image (This : Element_Type) return String;
package Optional.Values with Preelaborate is

   type Const_Ref;
   type Var_Ref;

   --------------
   -- Optional --
   --------------

   type Optional (Has_Element : Boolean) is tagged private;

   function "=" (L : Optional; R : Element_Type) return Boolean;
   function "=" (L : Element_Type; R : Optional) return Boolean;

   Empty : constant Optional;

   function Image (This : Optional) return String;

   function Unit (Element : Element_Type) return Optional;

   function Element (This : Optional) return Const_Ref'Class
     with Pre => This.Has_Element;

   function Reference (This : in out Optional) return Var_Ref'Class
     with Pre => This.Has_Element;

   ----------------
   -- Operations --
   ----------------

   function Flat_Map
     (This   : Optional;
      Mapper : access function (Element : Optional) return Optional)
      return Optional with
     Post => (if This.Has_Element and then Mapper /= null then
                Flat_Map'Result = Mapper (This)
              elsif This.Has_Element and then Mapper = null then
                Flat_Map'Result = This
              else
                Flat_Map'Result = Empty);

   function Map (This : Optional;
                 Mapper : access function (Element : Element_Type)
                                           return Element_Type)
                 return Optional with
     Post => (if This.Has_Element and then Mapper /= null then
                Map'Result.Element.Ptr.all = Mapper (This.Element.Ptr.all)
              elsif This.Has_Element and then Mapper = null then
                Map'Result = This
              else
                Map'Result = Empty);

   function Or_Else (This    : Optional;
                     Default : Element_Type)
                     return Element_Type with
     Post => (if This.Has_Element
              then Or_Else'Result = This.Element.Ptr.all
              else Or_Else'Result = Default);

   function Or_Raise (This   : Optional;
                      Ex_Id  : Ada.Exceptions.Exception_Id;
                      Ex_Msg : String := "")
                      return Element_Type with
     Post => (if This.Has_Element
              then Or_Raise'Result = This.Element.Ptr.all
              else raise Constraint_Error);
   --  Actually, Ex_Id will be raised, not CE

   function Filter (This      : Optional;
                    Condition : Boolean) return Optional with
     Post => (if Condition
                then Filter'Result = This
                else Filter'Result = Empty);

   ----------------
   -- References --
   ----------------

   type Const_Ref (Ptr : access constant Element_Type) is
     tagged limited null record with Implicit_Dereference => Ptr;

   function Image (This : Const_Ref) return String;

   type Var_Ref (Ptr : access Element_Type) is tagged limited null record
     with Implicit_Dereference => Ptr;

   function Image (This : Var_Ref) return String;

private

   package Holders is new Ada.Containers.Indefinite_Holders (Element_Type);

   type Optional (Has_Element : Boolean) is tagged record
      case Has_Element is
         when False => null;
         when True  => Element : Holders.Holder;
      end case;
   end record;

   Empty : constant Optional := (Has_Element => False);

   ---------
   -- "=" --
   ---------

   function "=" (L : Optional; R : Element_Type) return Boolean
   is (L.Has_Element and then L.Element.Constant_Reference = R);

   function "=" (L : Element_Type; R : Optional) return Boolean
   is (R = L);

   -------------
   -- Element --
   -------------

   function Element (This : Optional) return Const_Ref'Class
   is (Const_Ref'(Ptr => This.Element.Constant_Reference.Element));

   ------------
   -- Filter --
   ------------

   function Filter (This      : Optional;
                    Condition : Boolean) return Optional
   is (if Condition
       then This
       else Empty);

   --------------
   -- Flat_Map --
   --------------

   function Flat_Map
     (This   : Optional;
      Mapper : access function (Element : Optional) return Optional)
      return Optional
   is (if This.Has_Element and then Mapper /= null
       then Mapper (This)
       else This);

   -----------
   -- Image --
   -----------

   function Image (This : Optional) return String
   is (if not This.Has_Element
       then "[empty]"
       else "[value:"
            & Image (This.Element.Constant_Reference) & "]");

   function Image (This : Const_Ref) return String
   is (Image (This.Ptr.all));

   function Image (This : Var_Ref) return String
   is (Image (This.Ptr.all));

   ---------
   -- Map --
   ---------

   function Map (This : Optional;
                 Mapper : access function (Element : Element_Type)
                                           return Element_Type)
                 return Optional
   is (if This.Has_Element and then Mapper /= null
       then Unit (Mapper (This.Element.Element))
       else This);

   -------------
   -- Or_Else --
   -------------

   function Or_Else (This    : Optional;
                     Default : Element_Type)
                     return Element_Type
   is (if This.Has_Element
       then This.Element.Element
       else Default);

   ---------------
   -- Reference --
   ---------------

   function Reference (This : in out Optional) return Var_Ref'Class
   is (Var_Ref'(Ptr => This.Element.Reference.Element));

   ----------
   -- Unit --
   ----------

   function Unit (Element : Element_Type) return Optional
   is (Has_Element => True,
       Element     => Holders.To_Holder (Element));

end Optional.Values;

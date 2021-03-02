private with Ada.Containers.Indefinite_Holders;

generic
   type Elements (<>) is private;
package Optional.Values with Preelaborate is

   type Const_Ref (Ptr : access constant Elements) is limited null record
     with Implicit_Dereference => Ptr;

   type Var_Ref (Ptr : access Elements) is limited null record
     with Implicit_Dereference => Ptr;

   --------------
   -- Optional --
   --------------

   type Optional (Has_Element : Boolean) is tagged private;

   Empty : constant Optional;

   function Image (This : Optional) return String;

   function Unit (Element : Elements) return Optional;

   function Element (This : Optional) return Const_Ref
     with Pre => This.Has_Element;

   function Reference (This : in out Optional) return Var_Ref
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
                 Mapper : access function (Element : Elements) return Elements)
                 return Optional with
     Post => (if This.Has_Element and then Mapper /= null then
                Map'Result.Element = Mapper (This.Element)
              elsif This.Has_Element and then Mapper = null then
                Map'Result = This
              else
                Map'Result = Empty);

   function Or_Else (This    : Optional;
                     Default : Elements)
                     return Elements with
     Post => (if This.Has_Element
              then Or_Else'Result = This.Element
              else Or_Else'Result = Default);

private

   package Holders is new Ada.Containers.Indefinite_Holders (Elements);

   type Optional (Has_Element : Boolean) is tagged record
      case Has_Element is
         when False => null;
         when True  => Element : Holders.Holder;
      end case;
   end record;

   --  for Optional'Image use Image;

   Empty : constant Optional := (Has_Element => False);

   -------------
   -- Element --
   -------------

   function Element (This : Optional) return Const_Ref
   is (Ptr => This.Element.Constant_Reference.Element);

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
       then "[optional: empty]"
       else "[optional: "
            & This.Element.Constant_Reference.Element.all'Image & "]");

   ---------
   -- Map --
   ---------

   function Map (This : Optional;
                 Mapper : access function (Element : Elements) return Elements)
                 return Optional
   is (if This.Has_Element and then Mapper /= null
       then Unit (Mapper (This.Element.Element))
       else This);

   -------------
   -- Or_Else --
   -------------

   function Or_Else (This    : Optional;
                     Default : Elements)
                     return Elements
   is (if This.Has_Element
       then This.Element.Element
       else Default);

   ---------------
   -- Reference --
   ---------------

   function Reference (This : in out Optional) return Var_Ref
   is (Ptr => This.Element.Reference.Element);

   ----------
   -- Unit --
   ----------

   function Unit (Element : Elements) return Optional
   is (Has_Element => True,
       Element     => Holders.To_Holder (Element));

end Optional.Values;

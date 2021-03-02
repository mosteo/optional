private with Ada.Containers.Indefinite_Holders;

generic
   type Elements (<>) is private;
package Optional.Values with Preelaborate is

   --  Forward declarations to keep to Optional type at the top

   type Const_Ref;
   type Var_Ref;

   --------------
   -- Optional --
   --------------

   type Optional (Has_Element : Boolean) is tagged private;

   Empty : constant Optional;

   function Image (This : Optional) return String;

   function Unit (Element : Elements) return Optional;

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
                 Mapper : access function (Element : Elements) return Elements)
                 return Optional with
     Post => (if This.Has_Element and then Mapper /= null then
                Map'Result.Element.Ptr.all = Mapper (This.Element.Ptr.all)
              elsif This.Has_Element and then Mapper = null then
                Map'Result = This
              else
                Map'Result = Empty);

   function Or_Else (This    : Optional;
                     Default : Elements)
                     return Elements with
     Post => (if This.Has_Element
              then Or_Else'Result = This.Element.Ptr.all
              else Or_Else'Result = Default);

   ----------------
   -- References --
   ----------------

   type Const_Ref (Ptr : access constant Elements) is tagged limited null record
     with Implicit_Dereference => Ptr;

   function Image (This : Const_Ref) return String;

   type Var_Ref (Ptr : access Elements) is tagged limited null record
     with Implicit_Dereference => Ptr;

   function Image (This : Var_Ref) return String;

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

   function Element (This : Optional) return Const_Ref'Class
   is (Const_Ref'(Ptr => This.Element.Constant_Reference.Element));

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
            & This.Element.Constant_Reference.Element.all'Image & "]");

   function Image (This : Const_Ref) return String
   is (This.Ptr.all'Image);

   function Image (This : Var_Ref) return String
   is (This.Ptr.all'Image);

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

   function Reference (This : in out Optional) return Var_Ref'Class
   is (Var_Ref'(Ptr => This.Element.Reference.Element));

   ----------
   -- Unit --
   ----------

   function Unit (Element : Elements) return Optional
   is (Has_Element => True,
       Element     => Holders.To_Holder (Element));

end Optional.Values;

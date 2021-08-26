with Ada.Exceptions;
private with Ada.Finalization;

generic
   type Element_Type (<>) is private;
   with function Image (This : Element_Type) return String;
package Optional.Values with Preelaborate is

   --  Reference types

   type Const_Ref (Ptr : access constant Element_Type) is
   limited null record with Implicit_Dereference => Ptr;

   type Var_Ref (Ptr : access Element_Type) is
   limited null record with Implicit_Dereference => Ptr;

   --------------
   -- Optional --
   --------------

   type Optional is tagged private;

   function "=" (L : Optional; R : Element_Type) return Boolean with
     Post => "="'Result = (L.Has_Element and then L.Element = R);
   function "=" (L : Element_Type; R : Optional) return Boolean with
     Post => "="'Result = (R = L);

   Empty : constant Optional;

   function Has_Element (This : Optional) return Boolean;

   function Is_Empty (This : Optional) return Boolean with
     Post => Is_Empty'Result = not This.Has_Element;

   function Image (This : Optional) return String;
   --  Returns [empty] or [value: Element_Type'Image (This.Element)]

   function Unit (Element : Element_Type) return Optional with
     Post => Unit'Result.Element = Element;

   function Element (This : Optional) return Const_Ref
     with Pre => This.Has_Element;

   function Value (This : Optional) return Element_Type
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

   function Image (This : Const_Ref) return String;

   function Image (This : Var_Ref) return String;

private

   package AF renames Ada.Finalization;

   type Element_Access is access Element_Type;

   type Optional is new Ada.Finalization.Controlled
   with record
      Element : Element_Access; -- Holders still causing bugs
   end record;

   overriding procedure Adjust (This : in out Optional);

   overriding procedure Finalize (This : in out Optional);

   ---------
   -- "=" --
   ---------

   function "=" (L : Optional; R : Element_Type) return Boolean
   is (L.Has_Element and then L.Element.all = R);

   function "=" (L : Element_Type; R : Optional) return Boolean
   is (R = L);

   -------------
   -- Element --
   -------------

   function Element (This : Optional) return Const_Ref
   is (Const_Ref'(Ptr => This.Element));

   Empty : constant Optional := (AF.Controlled with Element => null);

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

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (This : Optional) return Boolean
   is (This.Element /= null);

   -----------
   -- Image --
   -----------

   function Image (This : Optional) return String
   is (if not This.Has_Element
       then "[empty]"
       else "[value:"
            & Image (This.Element.all) & "]");

   function Image (This : Const_Ref) return String
   is (Image (This.Ptr.all));

   function Image (This : Var_Ref) return String
   is (Image (This.Ptr.all));

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Optional) return Boolean
   is (not This.Has_Element);

   ---------
   -- Map --
   ---------

   function Map (This : Optional;
                 Mapper : access function (Element : Element_Type)
                                           return Element_Type)
                 return Optional
   is (if This.Has_Element and then Mapper /= null
       then Unit (Mapper (This.Element.all))
       else This);

   -------------
   -- Or_Else --
   -------------

   function Or_Else (This    : Optional;
                     Default : Element_Type)
                     return Element_Type
   is (if This.Has_Element
       then This.Element.all
       else Default);

   ---------------
   -- Reference --
   ---------------

   function Reference (This : in out Optional) return Var_Ref
   is (Var_Ref'(Ptr => This.Element));

   ----------
   -- Unit --
   ----------

   function Unit (Element : Element_Type) return Optional
   is (Ada.Finalization.Controlled with
       Element     => new Element_Type'(Element));

   -----------
   -- Value --
   -----------

   function Value (This : Optional) return Element_Type
   is (Element (This));

end Optional.Values;

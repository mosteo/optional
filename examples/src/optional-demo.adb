with Ada.Text_IO; use Ada.Text_IO;

with Optional.Values;

procedure Optional.Demo is

   function Image (S : String) return String is (S);

   package Opt_Strings is new Optional.Values (String);

begin
   Put_Line (Opt_Strings.Empty.Image);
   Put_Line (Opt_Strings.Empty.Or_Else ("default").Image);
   Put_Line (Opt_Strings.Unit ("hello").Image);
end Optional.Demo;

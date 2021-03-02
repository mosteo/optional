with Ada.Text_IO; use Ada.Text_IO;

with Optional.Values;

procedure Optional.Demo is

   package Opt_Strings is new Optional.Values (String);

begin
   Put_Line (Opt_Strings.Empty.Image);
   Put_Line (Opt_Strings.Empty.Or_Else ("default")'Image);
   Put_Line (Opt_Strings.Unit ("hello").Image);
   Put_Line (Opt_Strings.Unit ("hello").Element.Image);
end Optional.Demo;

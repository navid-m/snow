with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with System;                use System;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Interfaces;

#if Windows then
with Interfaces.C;
#end if;

package body Snow is
   procedure Initialize_Console is
#if Windows then
      use Interfaces.C;

      subtype UINT is unsigned;
      subtype BOOL is int;
      subtype DWORD is unsigned_long;
      subtype HANDLE is System.Address;

      function SetConsoleOutputCP (CodePage : UINT) return BOOL
      with
        Import        => True,
        Convention    => Stdcall,
        External_Name => "SetConsoleOutputCP";

      function SetConsoleCP (CodePage : UINT) return BOOL
      with
        Import        => True,
        Convention    => Stdcall,
        External_Name => "SetConsoleCP";

      function GetStdHandle (nStdHandle : Interfaces.C.long) return HANDLE
      with
        Import        => True,
        Convention    => Stdcall,
        External_Name => "GetStdHandle";

      function SetConsoleMode
        (hConsoleHandle : HANDLE; dwMode : DWORD) return BOOL
      with
        Import        => True,
        Convention    => Stdcall,
        External_Name => "SetConsoleMode";

      function GetConsoleMode
        (hConsoleHandle : HANDLE; lpMode : access DWORD) return BOOL
      with
        Import        => True,
        Convention    => Stdcall,
        External_Name => "GetConsoleMode";

      STD_OUTPUT_HANDLE                  : constant Interfaces.C.long := -11;
      ENABLE_VIRTUAL_TERMINAL_PROCESSING : constant DWORD := 16#0004#;
      CP_UTF8                            : constant UINT := 65001;
      Console_Handle                     : constant HANDLE :=
        GetStdHandle (STD_OUTPUT_HANDLE);
      Console_Mode                       : aliased DWORD := 0;
      Result                             : BOOL;
   begin
      Result := SetConsoleCP (CP_UTF8);
      Result := SetConsoleOutputCP (CP_UTF8);

      if Console_Handle /= System.Null_Address then
         if GetConsoleMode (Console_Handle, Console_Mode'Access) /= 0 then
            Console_Mode := Console_Mode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
            Result := SetConsoleMode (Console_Handle, Console_Mode);
         end if;
      end if;
#else
   begin
      null;
#end if;
   end Initialize_Console;

   Reset            : constant String := ASCII.ESC & "[0m";
   Bold             : constant String := ASCII.ESC & "[1m";
   Red              : constant String := ASCII.ESC & "[31m";
   Green            : constant String := ASCII.ESC & "[32m";
   Yellow           : constant String := ASCII.ESC & "[33m";
   Blue             : constant String := ASCII.ESC & "[34m";
   Cyan             : constant String := ASCII.ESC & "[36m";
   Box_Horizontal   : constant Wide_Wide_String := "─";
   Box_Vertical     : constant Wide_Wide_String := "│";
   Box_Top_Left     : constant Wide_Wide_String := "┌";
   Box_Top_Right    : constant Wide_Wide_String := "┐";
   Box_Bottom_Left  : constant Wide_Wide_String := "└";
   Box_Bottom_Right : constant Wide_Wide_String := "┘";
   Box_Cross        : constant Wide_Wide_String := "┼";
   Box_T_Down       : constant Wide_Wide_String := "┬";
   Box_T_Up         : constant Wide_Wide_String := "┴";
   Box_T_Right      : constant Wide_Wide_String := "├";
   Box_T_Left       : constant Wide_Wide_String := "┤";
   Tree_Branch      : constant Wide_Wide_String := "├── ";
   Tree_Last_Branch : constant Wide_Wide_String := "└── ";
   Tree_Vertical    : constant Wide_Wide_String := "│   ";
   Tree_Space       : constant Wide_Wide_String := "    ";

   function To_Wide_Wide_String (S : String) return Wide_Wide_String is
      Result : Wide_Wide_String (S'Range);
   begin
      for I in S'Range loop
         Result(I) := Wide_Wide_Character'Val(Character'Pos(S(I)));
      end loop;
      return Result;
   end To_Wide_Wide_String;

   procedure Print_Title (Content : Unbounded_String) is
   begin
      Print_Title (To_String (Content));
   end Print_Title;

   procedure Print_Title (Content : String) is
      Line_Length : constant Natural := Content'Length + 4;
      Line        : constant String := (1 .. Line_Length => '=');
   begin
      New_Line;
      Ada.Text_IO.Put_Line (Bold & Cyan & Line & Reset);
      Ada.Text_IO.Put_Line (Bold & Cyan & "  " & Content & "  " & Reset);
      Ada.Text_IO.Put_Line (Bold & Cyan & Line & Reset);
      New_Line;
   end Print_Title;

   procedure Toast (Message : Unbounded_String; Level : Toast_Level := Info) is
   begin
      Toast (To_String (Message), Level);
   end Toast;

   procedure Toast (Message : String; Level : Toast_Level := Info) is
      Icon   : Wide_Wide_String (1 .. 3);
      Color  : Unbounded_String;
      Prefix : Unbounded_String;
   begin
      case Level is
         when Info    =>
            Icon := " ℹ ";
            Color := To_Unbounded_String (Blue);
            Prefix := To_Unbounded_String ("INFO");

         when Success =>
            Icon := " ✓ ";
            Color := To_Unbounded_String (Green);
            Prefix := To_Unbounded_String ("SUCCESS");

         when Warning =>
            Icon := " ⚠ ";
            Color := To_Unbounded_String (Yellow);
            Prefix := To_Unbounded_String ("WARNING");

         when Error   =>
            Icon := " ✗ ";
            Color := To_Unbounded_String (Red);
            Prefix := To_Unbounded_String ("ERROR");
      end case;

      Ada.Text_IO.Put (To_String (Color) & Bold);
      Put (Icon);
      Ada.Text_IO.Put_Line
        ("[" & To_String (Prefix) & "]" & Reset & " " & Message);
   end Toast;

   procedure Print_Tree_Node
     (Label   : Unbounded_String;
      Depth   : Natural := 0;
      Is_Last : Boolean := True) is
   begin
      Print_Tree_Node (To_String (Label), Depth, Is_Last);
   end Print_Tree_Node;

   procedure Print_Tree_Node
     (Label : String; Depth : Natural := 0; Is_Last : Boolean := True) is
   begin
      for I in 1 .. Depth loop
         if I < Depth then
            Put (Tree_Vertical);

         else
            if Is_Last then
               Put (Tree_Last_Branch);

            else
               Put (Tree_Branch);
            end if;
         end if;
      end loop;

      if Depth = 0 then
         Ada.Text_IO.Put_Line (Bold & Green & Label & Reset);
      else
         Ada.Text_IO.Put_Line (Label);
      end if;
   end Print_Tree_Node;

   procedure Add_Header (T : in out Table; Headers : String_Vectors.Vector) is
   begin
      T.Headers := Headers;
      T.Has_Header := True;

      if T.Alignments /= null then
         T.Alignments := null;
      end if;

      T.Alignments := new Alignment_Array (0 .. Natural (Headers.Length) - 1);
      for I in T.Alignments'Range loop
         T.Alignments (I) := Left;
      end loop;
   end Add_Header;

   procedure Add_Row (T : in out Table; Row : String_Vectors.Vector) is
   begin
      T.Rows.Append (Row);
   end Add_Row;

   procedure Set_Column_Alignment
     (T : in out Table; Column : Natural; Align : Alignment) is
   begin
      if T.Alignments /= null and then Column in T.Alignments'Range then
         T.Alignments (Column) := Align;
      end if;
   end Set_Column_Alignment;

   procedure Clear (T : in out Table) is
   begin
      T.Headers.Clear;
      T.Rows.Clear;
      T.Has_Header := False;
      if T.Alignments /= null then
         T.Alignments := null;
      end if;
   end Clear;

   function Pad_String
     (S : String; Width : Natural; Align : Alignment) return String
   is
      use Ada.Strings.Fixed;
      Len : constant Natural := S'Length;
   begin
      if Len >= Width then
         return S;
      end if;

      case Align is
         when Left   =>
            return S & (1 .. Width - Len => ' ');

         when Right  =>
            return (1 .. Width - Len => ' ') & S;

         when Center =>
            declare
               Left_Pad  : constant Natural := (Width - Len) / 2;
               Right_Pad : constant Natural := Width - Len - Left_Pad;
            begin
               return (1 .. Left_Pad => ' ') & S & (1 .. Right_Pad => ' ');
            end;
      end case;
   end Pad_String;

   procedure Print (T : Table) is
      Col_Count  : Natural := 0;
      type Width_Array is array (Natural range <>) of Natural;
      type Width_Array_Access is access Width_Array;
      Col_Widths : Width_Array_Access;
   begin
      if T.Has_Header then
         Col_Count := Natural (T.Headers.Length);
      elsif not T.Rows.Is_Empty then
         Col_Count := Natural (T.Rows.First_Element.Length);
      else
         return;
      end if;

      if Col_Count = 0 then
         return;
      end if;

      Col_Widths := new Width_Array (0 .. Col_Count - 1);
      for I in Col_Widths'Range loop
         Col_Widths (I) := 0;
      end loop;
      if T.Has_Header then
         for I in 0 .. Natural (T.Headers.Length) - 1 loop
            declare
               Header : constant String := To_String (T.Headers.Element (I));
            begin
               if Header'Length > Col_Widths (I) then
                  Col_Widths (I) := Header'Length;
               end if;
            end;
         end loop;
      end if;

      for Row of T.Rows loop
         for I in 0 .. Natural (Row.Length) - 1 loop
            declare
               Cell : constant String := To_String (Row.Element (I));
            begin
               if I < Col_Count and then Cell'Length > Col_Widths (I) then
                  Col_Widths (I) := Cell'Length;
               end if;
            end;
         end loop;
      end loop;

      Put (Box_Top_Left);
      for I in Col_Widths'Range loop
         for J in 1 .. Col_Widths (I) + 2 loop
            Put (Box_Horizontal);
         end loop;
         if I < Col_Widths'Last then
            Put (Box_T_Down);
         end if;
      end loop;
      Put_Line (Box_Top_Right);

      if T.Has_Header then
         Put (Box_Vertical);
         for I in 0 .. Natural (T.Headers.Length) - 1 loop
            declare
               Header : constant String := To_String (T.Headers.Element (I));
               Align  : constant Alignment :=
                 (if T.Alignments /= null and then I in T.Alignments'Range
                  then T.Alignments (I)
                  else Left);
               Padded : constant String :=
                 Pad_String (Header, Col_Widths (I), Align);
            begin
               Ada.Text_IO.Put (" " & Bold & Padded & Reset & " ");
               Put (Box_Vertical);
            end;
         end loop;
         New_Line;

         Put (Box_T_Right);
         for I in Col_Widths'Range loop
            for J in 1 .. Col_Widths (I) + 2 loop
               Put (Box_Horizontal);
            end loop;
            if I < Col_Widths'Last then
               Put (Box_Cross);
            end if;
         end loop;
         Put_Line (Box_T_Left);
      end if;

      for Row of T.Rows loop
         Put (Box_Vertical);
         for I in 0 .. Col_Count - 1 loop
            declare
               Cell   : constant String :=
                 (if I < Natural (Row.Length)
                  then To_String (Row.Element (I))
                  else "");
               Align  : constant Alignment :=
                 (if T.Alignments /= null and then I in T.Alignments'Range
                  then T.Alignments (I)
                  else Left);
               Padded : constant String :=
                 Pad_String (Cell, Col_Widths (I), Align);
            begin
               Ada.Text_IO.Put (" " & Padded & " ");
               Put (Box_Vertical);
            end;
         end loop;
         New_Line;
      end loop;

      Put (Box_Bottom_Left);
      for I in Col_Widths'Range loop
         for J in 1 .. Col_Widths (I) + 2 loop
            Put (Box_Horizontal);
         end loop;
         if I < Col_Widths'Last then
            Put (Box_T_Up);
         end if;
      end loop;
      Put_Line (Box_Bottom_Right);

   end Print;

   function Make_Vector (Items : String) return String_Vectors.Vector is
      V : String_Vectors.Vector;
   begin
      V.Append (To_Unbounded_String (Items));
      return V;
   end Make_Vector;

   function "&"
     (Left : String_Vectors.Vector; Right : String)
      return String_Vectors.Vector
   is
      Result : String_Vectors.Vector := Left;
   begin
      Result.Append (To_Unbounded_String (Right));
      return Result;
   end "&";

   procedure Add_Data_Series 
     (Chart : in out Bar_Chart; 
      Label : String; 
      Value : Natural) is
   begin
      Chart.Data.Append (Data_Point'(To_Unbounded_String(Label), Value));
      if Value > Chart.Max_Value then
         Chart.Max_Value := Value;
      end if;
   end Add_Data_Series;
   
   procedure Set_Title (Chart : in out Bar_Chart; Title : String) is
   begin
      Chart.Title := To_Unbounded_String(Title);
   end Set_Title;
   
   procedure Print (Chart : Bar_Chart) is
      Bar_Width : constant Natural := 30;
      Bar_Char  : constant Wide_Wide_Character := '█';
      Scale     : Float;
      Max_Label_Length : Natural := 0;
   begin
      if Chart.Data.Is_Empty then
         return;
      end if;
      
      for Point of Chart.Data loop
         if Length(Point.Label) > Max_Label_Length then
            Max_Label_Length := Length(Point.Label);
         end if;
      end loop;
      
      if Chart.Max_Value = 0 then
         Scale := 0.0;
      else
         Scale := Float(Bar_Width) / Float(Chart.Max_Value);
      end if;
      
      if Chart.Title /= Null_Unbounded_String then
         Put_Line(To_Wide_Wide_String(Bold) & To_Wide_Wide_String(Cyan) & 
                  To_Wide_Wide_String(To_String(Chart.Title)) & 
                  To_Wide_Wide_String(Reset));
         New_Line;
      end if;
      
      for Point of Chart.Data loop
         declare
            Bar_Length : constant Natural := 
              Natural(Float(Point.Value) * Scale);
            Padded_Label : constant String := 
              To_String(Point.Label) & 
              (1 .. Max_Label_Length - Length(Point.Label) => ' ');
            Bar : constant Wide_Wide_String := 
              (for I in 1 .. Bar_Length => Bar_Char) & 
              To_Wide_Wide_String(" " & Natural'Image(Point.Value));
         begin
            Put_Line(
              To_Wide_Wide_String(Padded_Label & ": ") & 
              To_Wide_Wide_String(Green) & Bar & To_Wide_Wide_String(Reset));
         end;
      end loop;
   end Print;
   
   procedure Clear (Chart : in out Bar_Chart) is
   begin
      Chart.Title := Null_Unbounded_String;
      Chart.Data.Clear;
      Chart.Max_Value := 0;
   end Clear;

end Snow;
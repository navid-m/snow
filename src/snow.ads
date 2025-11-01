with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package Snow is
   procedure Initialize_Console;

   type Toast_Level is (Info, Success, Warning, Error);
   type Alignment is (Left, Center, Right);

   package String_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Unbounded_String);

   package Row_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => String_Vectors.Vector,
        "="          => String_Vectors."=");

   procedure Print_Title (Content : Unbounded_String);
   procedure Print_Title (Content : String);
   procedure Toast (Message : String; Level : Toast_Level := Info);
   procedure Toast (Message : Unbounded_String; Level : Toast_Level := Info);
   procedure Print_Tree_Node
     (Label : String; Depth : Natural := 0; Is_Last : Boolean := True);
   procedure Print_Tree_Node
     (Label   : Unbounded_String;
      Depth   : Natural := 0;
      Is_Last : Boolean := True);

   type Table is tagged private;

   procedure Add_Header (T : in out Table; Headers : String_Vectors.Vector);
   procedure Add_Row (T : in out Table; Row : String_Vectors.Vector);
   procedure Set_Column_Alignment
     (T : in out Table; Column : Natural; Align : Alignment);
   procedure Print (T : Table);
   procedure Clear (T : in out Table);

   type Bar_Chart is tagged private;

   procedure Add_Data_Series
     (Chart : in out Bar_Chart; Label : String; Value : Natural);
   procedure Set_Title (Chart : in out Bar_Chart; Title : String);
   procedure Print (Chart : Bar_Chart);
   procedure Clear (Chart : in out Bar_Chart);

   type Sparkline is tagged private;

   procedure Add_Data_Series (Spark : in out Sparkline; Value : Natural);
   procedure Add_Data_Point (Spark : in out Sparkline; Value : Natural);
   procedure Set_Width (Spark : in out Sparkline; Width : Natural);

   function Get_String (Spark : Sparkline) return Wide_Wide_String;

   procedure Print (Spark : Sparkline);
   procedure Clear (Spark : in out Sparkline);
   procedure Print_Bounding_Box (Content : String; Title : String := "");
   procedure Print_Bounding_Box
     (Content : Unbounded_String; Title : String := "");

   function Make_Vector (Items : String) return String_Vectors.Vector;
   function "&"
     (Left : String_Vectors.Vector; Right : String)
      return String_Vectors.Vector;
   function To_Wide_Wide_String (S : String) return Wide_Wide_String;

private
   type Alignment_Array is array (Natural range <>) of Alignment;
   type Alignment_Array_Access is access Alignment_Array;
   type Table is tagged record
      Headers    : String_Vectors.Vector;
      Rows       : Row_Vectors.Vector;
      Has_Header : Boolean := False;
      Alignments : Alignment_Array_Access := null;
   end record;

   type Data_Point is record
      Label : Unbounded_String;
      Value : Natural;
   end record;

   package Data_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Data_Point);

   type Bar_Chart is tagged record
      Title     : Unbounded_String;
      Data      : Data_Vectors.Vector;
      Max_Value : Natural := 0;
   end record;

   type Sparkline is tagged record
      Data      : Data_Vectors.Vector;
      Max_Value : Natural := 0;
      Width     : Natural := 20;
   end record;
end Snow;

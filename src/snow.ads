with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

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
     (Chart : in out Bar_Chart; 
      Label : String; 
      Value : Natural);
   procedure Set_Title (Chart : in out Bar_Chart; Title : String);
   procedure Print (Chart : Bar_Chart);
   procedure Clear (Chart : in out Bar_Chart);

   function Make_Vector (Items : String) return String_Vectors.Vector;
   function "&"
     (Left : String_Vectors.Vector; Right : String)
      return String_Vectors.Vector;

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
      Title  : Unbounded_String;
      Data   : Data_Vectors.Vector;
      Max_Value : Natural := 0;
   end record;

end Snow;

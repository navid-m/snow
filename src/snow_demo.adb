with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Snow;                  use Snow;

procedure Snow_Demo is
   My_Table : Snow.Table;

   function Row (C1, C2, C3 : String) return String_Vectors.Vector is
      V : String_Vectors.Vector;
   begin
      V.Append (To_Unbounded_String (C1));
      V.Append (To_Unbounded_String (C2));
      V.Append (To_Unbounded_String (C3));
      return V;
   end Row;

begin
   Initialize_Console;
   Print_Title ("Snow UI Library Demo");
   Print_Title ("Toast Notifications");
   Toast ("This is an informational message", Info);
   Toast ("Operation completed successfully!", Success);
   Toast ("This action may have consequences", Warning);
   Toast ("An error has occurred", Error);
   New_Line;
   Print_Title ("Tree Structure");
   Print_Tree_Node ("Project Root", 0, False);
   Print_Tree_Node ("src/", 1, False);
   Print_Tree_Node ("main.adb", 2, False);
   Print_Tree_Node ("snow.ads", 2, False);
   Print_Tree_Node ("snow-body.adb", 2, True);
   Print_Tree_Node ("docs/", 1, False);
   Print_Tree_Node ("README.md", 2, True);
   Print_Tree_Node ("build/", 1, True);
   Print_Tree_Node ("output.exe", 2, True);
   New_Line;
   Print_Title ("Table Example - Employee Data");
   declare
      Headers : String_Vectors.Vector;
   begin
      Headers.Append (To_Unbounded_String ("Name"));
      Headers.Append (To_Unbounded_String ("Department"));
      Headers.Append (To_Unbounded_String ("Salary"));
      Add_Header (My_Table, Headers);
   end;

   Set_Column_Alignment (My_Table, 0, Left);    -- Name
   Set_Column_Alignment (My_Table, 1, Center);  -- Department
   Set_Column_Alignment (My_Table, 2, Right);   -- Salary

   Add_Row (My_Table, Row ("Alice Johnson", "Engineering", "$95,000"));
   Add_Row (My_Table, Row ("Bob Smith", "Marketing", "$72,000"));
   Add_Row (My_Table, Row ("Carol White", "Engineering", "$105,000"));
   Add_Row (My_Table, Row ("David Brown", "Sales", "$68,000"));
   Add_Row (My_Table, Row ("Eve Davis", "HR", "$78,000"));

   Print (My_Table);
   New_Line;

   Print_Title ("Sparkline Example");
   declare
      Spark : Snow.Sparkline;
   begin
      Add_Data_Point (Spark, 5);
      Add_Data_Point (Spark, 8);
      Add_Data_Point (Spark, 12);
      Add_Data_Point (Spark, 7);
      Add_Data_Point (Spark, 15);
      Add_Data_Point (Spark, 10);
      Add_Data_Point (Spark, 3);
      Add_Data_Point (Spark, 9);
      Print (Spark);
   end;

   Print_Title ("Table Example - Project Status");
   Clear (My_Table);

   declare
      Headers : String_Vectors.Vector;
   begin
      Headers.Append (To_Unbounded_String ("Task"));
      Headers.Append (To_Unbounded_String ("Status"));
      Headers.Append (To_Unbounded_String ("Priority"));
      Add_Header (My_Table, Headers);
   end;

   Set_Column_Alignment (My_Table, 0, Left);
   Set_Column_Alignment (My_Table, 1, Center);
   Set_Column_Alignment (My_Table, 2, Center);

   Add_Row (My_Table, Row ("Implement UI Library", "Complete", "High"));
   Add_Row (My_Table, Row ("Write Documentation", "In Progress", "Medium"));
   Add_Row (My_Table, Row ("Add Unit Tests", "Pending", "High"));
   Add_Row (My_Table, Row ("Code Review", "Pending", "Low"));

   Print (My_Table);
   New_Line;

   Print_Title ("Bar Chart Example");
   declare
      Chart : Snow.Bar_Chart;
   begin
      Set_Title (Chart, "Monthly Sales");
      Add_Data_Series (Chart, "January", 12000);
      Add_Data_Series (Chart, "February", 18000);
      Add_Data_Series (Chart, "March", 15000);
      Add_Data_Series (Chart, "April", 21000);
      Add_Data_Series (Chart, "May", 19000);
      Print (Chart);
   end;
   New_Line;

   Toast ("Demo completed successfully!", Success);

end Snow_Demo;

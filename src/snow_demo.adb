with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Snow; use Snow;

procedure Snow_Demo is

   -- Table for demonstration
   My_Table : Snow.Table;

   -- Helper to create a row
   function Row (C1, C2, C3 : String) return String_Vectors.Vector is
      V : String_Vectors.Vector;
   begin
      V.Append (To_Unbounded_String (C1));
      V.Append (To_Unbounded_String (C2));
      V.Append (To_Unbounded_String (C3));
      return V;
   end Row;

begin

   -- Initialize console for UTF-8 output on Windows
   Initialize_Console;

   -- Demonstrate Print_Title
   Print_Title ("Snow UI Library Demo");

   -- Demonstrate Toast notifications
   Print_Title ("Toast Notifications");
   Toast ("This is an informational message", Info);
   Toast ("Operation completed successfully!", Success);
   Toast ("This action may have consequences", Warning);
   Toast ("An error has occurred", Error);
   New_Line;

   -- Demonstrate Tree printing
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

   -- Demonstrate Table printing
   Print_Title ("Table Example - Employee Data");

   -- Create headers
   declare
      Headers : String_Vectors.Vector;
   begin
      Headers.Append (To_Unbounded_String ("Name"));
      Headers.Append (To_Unbounded_String ("Department"));
      Headers.Append (To_Unbounded_String ("Salary"));
      Add_Header (My_Table, Headers);
   end;

   -- Set column alignments
   Set_Column_Alignment (My_Table, 0, Left);    -- Name
   Set_Column_Alignment (My_Table, 1, Center);  -- Department
   Set_Column_Alignment (My_Table, 2, Right);   -- Salary

   -- Add data rows
   Add_Row (My_Table, Row ("Alice Johnson", "Engineering", "$95,000"));
   Add_Row (My_Table, Row ("Bob Smith", "Marketing", "$72,000"));
   Add_Row (My_Table, Row ("Carol White", "Engineering", "$105,000"));
   Add_Row (My_Table, Row ("David Brown", "Sales", "$68,000"));
   Add_Row (My_Table, Row ("Eve Davis", "HR", "$78,000"));

   -- Print the table
   Print (My_Table);
   New_Line;

   -- Another table example
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

   -- Final message
   Toast ("Demo completed successfully!", Success);

end Snow_Demo;

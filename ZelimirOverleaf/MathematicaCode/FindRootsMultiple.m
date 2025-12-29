(* Content-type: application/vnd.wolfram.mathematica *)

(*** Wolfram Notebook File ***)
(* http://www.wolfram.com/nb *)

(* CreatedBy='Mathematica 11.0' *)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[       158,          7]
NotebookDataLength[     11756,        293]
NotebookOptionsPosition[     11091,        274]
NotebookOutlinePosition[     11484,        290]
CellTagsIndexPosition[     11441,        287]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{

Cell[CellGroupData[{
Cell[BoxData[
 RowBox[{
  RowBox[{"(*", "\[IndentingNewLine]", 
   RowBox[{"Find", " ", "Multiple", " ", "Roots"}], "\[IndentingNewLine]", 
   "*)"}], "\[IndentingNewLine]", "\[IndentingNewLine]", 
  RowBox[{
   RowBox[{"BeginPackage", "[", "\"\<FindRootsMultiple`\>\"", "]"}], 
   "\[IndentingNewLine]", "\[IndentingNewLine]", 
   RowBox[{"Begin", "[", "\"\<`Private`\>\"", "]"}], " ", 
   RowBox[{"(*", 
    RowBox[{"Begin", " ", "Private", " ", "Context"}], "*)"}], 
   "\[IndentingNewLine]", "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"Options", "@", "FindRoots"}], "=", 
     RowBox[{"Sort", "@", 
      RowBox[{"Join", "[", 
       RowBox[{
        RowBox[{"Options", "@", "FindRoot"}], ",", 
        RowBox[{"{", 
         RowBox[{
          RowBox[{"MaxRecursion", "\[Rule]", "Automatic"}], ",", 
          RowBox[{"PerformanceGoal", "\[RuleDelayed]", "$PerformanceGoal"}], 
          ",", 
          RowBox[{"PlotPoints", "\[Rule]", "Automatic"}], ",", 
          RowBox[{"Debug", "\[Rule]", "False"}], ",", 
          RowBox[{"ZeroTolerance", "\[Rule]", 
           RowBox[{"10", "^", 
            RowBox[{"-", "2"}]}]}]}], "}"}]}], "]"}]}]}], ";"}], 
   "\[IndentingNewLine]", "\n", 
   RowBox[{
    RowBox[{"FindRoots", "[", 
     RowBox[{"fun_", ",", 
      RowBox[{"{", 
       RowBox[{"var_", ",", "min_", ",", "max_"}], "}"}], ",", 
      RowBox[{"opts", ":", 
       RowBox[{"OptionsPattern", "[", "]"}]}]}], "]"}], ":=", 
    RowBox[{"Module", "[", 
     RowBox[{
      RowBox[{"{", 
       RowBox[{
       "PlotRules", ",", "RootRules", ",", "g", ",", "g2", ",", "pts", ",", 
        "pts2", ",", "lpts", ",", "F", ",", "sol"}], "}"}], ",", 
      RowBox[{"(*", 
       RowBox[{"Extract", " ", "the", " ", "Options"}], "*)"}], 
      RowBox[{
       RowBox[{"PlotRules", "=", 
        RowBox[{"Sequence", "@@", 
         RowBox[{"FilterRules", "[", 
          RowBox[{
           RowBox[{"Join", "[", 
            RowBox[{
             RowBox[{"{", "opts", "}"}], ",", 
             RowBox[{"Options", "@", "FindRoots"}]}], "]"}], ",", 
           RowBox[{"Options", "@", "Plot"}]}], "]"}]}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"RootRules", "=", 
        RowBox[{"Sequence", "@@", 
         RowBox[{"FilterRules", "[", 
          RowBox[{
           RowBox[{"Join", "[", 
            RowBox[{
             RowBox[{"{", "opts", "}"}], ",", 
             RowBox[{"Options", "@", "FindRoots"}]}], "]"}], ",", 
           RowBox[{"Options", "@", "FindRoot"}]}], "]"}]}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
         RowBox[{
         "Plot", " ", "the", " ", "function", " ", "and", " ", "\"\<mesh\>\"",
           " ", "the", " ", "point", " ", "with", " ", "y"}], "-", 
         RowBox[{"coordinate", " ", "0"}]}], "*)"}], 
       RowBox[{"g", "=", 
        RowBox[{"Normal", "@", 
         RowBox[{"Plot", "[", 
          RowBox[{"fun", ",", 
           RowBox[{"{", 
            RowBox[{"var", ",", "min", ",", "max"}], "}"}], ",", 
           RowBox[{"MeshFunctions", "\[Rule]", 
            RowBox[{"(", 
             RowBox[{"#2", "&"}], ")"}]}], ",", 
           RowBox[{"Mesh", "\[Rule]", 
            RowBox[{"{", 
             RowBox[{"{", "0", "}"}], "}"}]}], ",", 
           RowBox[{"Method", "\[Rule]", "Automatic"}], ",", 
           RowBox[{"Evaluate", "@", "PlotRules"}]}], "]"}]}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{"Get", " ", "the", " ", "meshes", " ", "zeros"}], "*)"}], 
       RowBox[{"pts", "=", 
        RowBox[{"Cases", "[", 
         RowBox[{"g", ",", 
          RowBox[{
           RowBox[{"Point", "[", "p_", "]"}], "\[RuleDelayed]", 
           RowBox[{"SetPrecision", "[", 
            RowBox[{
             RowBox[{"p", "[", 
              RowBox[{"[", "1", "]"}], "]"}], ",", 
             RowBox[{"OptionValue", "@", "WorkingPrecision"}]}], "]"}]}], ",",
           "Infinity"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{"Get", " ", "all", " ", "plot", " ", "points"}], "*)"}], 
       RowBox[{"lpts", "=", 
        RowBox[{"Join", "@@", 
         RowBox[{"Cases", "[", 
          RowBox[{"g", ",", 
           RowBox[{
            RowBox[{"Line", "[", "p_", "]"}], "\[RuleDelayed]", 
            RowBox[{"SetPrecision", "[", 
             RowBox[{"p", ",", 
              RowBox[{"OptionValue", "@", "WorkingPrecision"}]}], "]"}]}], 
           ",", "Infinity"}], "]"}]}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
        "Derive", " ", "the", " ", "interpolated", " ", "data", " ", "to", 
         " ", "find", " ", "other", " ", "zeros"}], "*)"}], 
       RowBox[{"F", "=", 
        RowBox[{"Interpolation", "[", 
         RowBox[{"lpts", ",", 
          RowBox[{"InterpolationOrder", "\[Rule]", "2"}]}], "]"}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"g2", "=", 
        RowBox[{"Normal", "@", 
         RowBox[{"Plot", "[", 
          RowBox[{
           RowBox[{"Evaluate", "@", 
            RowBox[{"D", "[", 
             RowBox[{
              RowBox[{"F", "@", "var"}], ",", "var"}], "]"}]}], ",", 
           RowBox[{"{", 
            RowBox[{"var", ",", "min", ",", "max"}], "}"}], ",", 
           RowBox[{"MeshFunctions", "\[Rule]", 
            RowBox[{"(", 
             RowBox[{"#2", "&"}], ")"}]}], ",", 
           RowBox[{"Mesh", "\[Rule]", 
            RowBox[{"{", 
             RowBox[{"{", "0", "}"}], "}"}]}], ",", 
           RowBox[{"Method", "\[Rule]", "Automatic"}], ",", 
           RowBox[{"Evaluate", "@", "PlotRules"}]}], "]"}]}]}], ";", 
       "\[IndentingNewLine]", 
       RowBox[{"(*", 
        RowBox[{
        "Get", " ", "the", " ", "meshes", " ", "zeros", " ", "and", " ", 
         "retain", " ", "only", " ", "small", " ", "ones"}], "*)"}], 
       RowBox[{"pts2", "=", 
        RowBox[{"Cases", "[", 
         RowBox[{"g2", ",", 
          RowBox[{
           RowBox[{"Point", "[", "p_", "]"}], "\[RuleDelayed]", 
           RowBox[{"SetPrecision", "[", 
            RowBox[{
             RowBox[{"p", "[", 
              RowBox[{"[", "1", "]"}], "]"}], ",", 
             RowBox[{"OptionValue", "@", "WorkingPrecision"}]}], "]"}]}], ",",
           "Infinity"}], "]"}]}], ";", "\[IndentingNewLine]", 
       RowBox[{"pts2", "=", 
        RowBox[{"Select", "[", 
         RowBox[{"pts2", ",", 
          RowBox[{
           RowBox[{
            RowBox[{"Abs", "[", 
             RowBox[{"F", "@", "#"}], "]"}], "<", 
            RowBox[{"OptionValue", "@", "ZeroTolerance"}]}], "&"}]}], "]"}]}],
        ";", "\[IndentingNewLine]", 
       RowBox[{"pts", "=", 
        RowBox[{"Join", "[", 
         RowBox[{"pts", ",", "pts2"}], "]"}]}], ";", 
       RowBox[{"(*", 
        RowBox[{"Join", " ", "all", " ", "zeros"}], "*)"}], 
       RowBox[{"(*", 
        RowBox[{
        "Refine", " ", "zeros", " ", "by", " ", "passing", " ", "each", " ", 
         "point", " ", "through", " ", "FindRoot"}], "*)"}], 
       RowBox[{"If", "[", 
        RowBox[{
         RowBox[{
          RowBox[{"Length", "@", "pts"}], ">", "0"}], ",", 
         RowBox[{
          RowBox[{"pts", "=", 
           RowBox[{"Map", "[", 
            RowBox[{
             RowBox[{
              RowBox[{"FindRoot", "[", 
               RowBox[{"fun", ",", 
                RowBox[{"{", 
                 RowBox[{"var", ",", "#"}], "}"}], ",", 
                RowBox[{"Evaluate", "@", "RootRules"}]}], "]"}], "&"}], ",", 
             "pts"}], "]"}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"sol", "=", 
           RowBox[{"Union", "@", 
            RowBox[{"Select", "[", 
             RowBox[{"pts", ",", 
              RowBox[{
               RowBox[{"min", "\[LessEqual]", 
                RowBox[{"Last", "@", 
                 RowBox[{"Last", "@", "#"}]}], "\[LessEqual]", "max"}], 
               "&"}]}], "]"}]}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{"For", " ", "debug", " ", "purposes"}], "*)"}], 
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"OptionValue", "@", "Debug"}], ",", 
            RowBox[{"Print", "@", 
             RowBox[{"Show", "[", 
              RowBox[{"g", ",", 
               RowBox[{"Graphics", "@", 
                RowBox[{"{", 
                 RowBox[{
                  RowBox[{"PointSize", "@", "0.02"}], ",", "Red", ",", 
                  RowBox[{"Point", "[", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{"var", ",", "fun"}], "}"}], "/.", "sol"}], 
                   "]"}]}], "}"}]}]}], "]"}]}]}], "]"}], ";", 
          "\[IndentingNewLine]", "sol"}], ",", 
         RowBox[{
          RowBox[{"If", "[", 
           RowBox[{
            RowBox[{"OptionValue", "@", "Debug"}], ",", 
            RowBox[{"Print", "@", "g"}]}], "]"}], ";", "\[IndentingNewLine]", 
          
          RowBox[{"{", "}"}]}]}], "]"}]}]}], "]"}]}], "\[IndentingNewLine]", 
   "\[IndentingNewLine]", 
   RowBox[{"End", "[", "]"}], " ", 
   RowBox[{"(*", 
    RowBox[{"End", " ", "Private", " ", "Context"}], "*)"}], 
   "\[IndentingNewLine]", 
   RowBox[{"EndPackage", "[", "]"}]}]}]], "Input",
 CellChangeTimes->{{3.773620662157366*^9, 3.773620683192981*^9}, {
   3.826324816386272*^9, 3.8263248177250957`*^9}, {3.826324873219413*^9, 
   3.826324936627809*^9}, {3.8263275518874683`*^9, 3.8263275519815083`*^9}, {
   3.826476334255906*^9, 3.826476338875228*^9}, {3.826928439061352*^9, 
   3.826928439307886*^9}, {3.82692911571952*^9, 3.826929129858363*^9}, 
   3.826929222519158*^9},
 CellLabel->"In[1]:=",ExpressionUUID->"d4e5c97f-ee70-45ce-98b6-8ca7723c302e"],

Cell[BoxData["\<\"FindRootsMultiple`\"\>"], "Output",
 CellChangeTimes->{3.8269290018868933`*^9, 3.826929181741427*^9, 
  3.826929224573978*^9, 3.828202149029752*^9, 3.8282022902131863`*^9},
 CellLabel->"Out[1]=",ExpressionUUID->"372c6d70-8ac9-4321-9f00-1387577d5b13"],

Cell[BoxData["\<\"FindRootsMultiple`Private`\"\>"], "Output",
 CellChangeTimes->{3.8269290018868933`*^9, 3.826929181741427*^9, 
  3.826929224573978*^9, 3.828202149029752*^9, 3.8282022902160873`*^9},
 CellLabel->"Out[2]=",ExpressionUUID->"bd2ddcb6-e134-4d3d-8ec3-54a0ac4c9fac"],

Cell[BoxData["\<\"FindRootsMultiple`Private`\"\>"], "Output",
 CellChangeTimes->{3.8269290018868933`*^9, 3.826929181741427*^9, 
  3.826929224573978*^9, 3.828202149029752*^9, 3.828202290220058*^9},
 CellLabel->"Out[5]=",ExpressionUUID->"40260aa7-1d11-4cf7-b643-481aba628c78"]
}, Open  ]]
},
WindowSize->{808, 591},
WindowMargins->{{Automatic, 169}, {Automatic, 0}},
FrontEndVersion->"12.1 for Mac OS X x86 (64-bit) (June 19, 2020)",
StyleDefinitions->"Default.nb",
ExpressionUUID->"51d4f1dd-0139-4b8d-97ec-0c1a0d351d80"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{}
*)
(*CellTagsIndex
CellTagsIndex->{}
*)
(*NotebookFileOutline
Notebook[{
Cell[CellGroupData[{
Cell[580, 22, 9668, 234, 976, "Input",ExpressionUUID->"d4e5c97f-ee70-45ce-98b6-8ca7723c302e"],
Cell[10251, 258, 268, 3, 34, "Output",ExpressionUUID->"372c6d70-8ac9-4321-9f00-1387577d5b13"],
Cell[10522, 263, 276, 3, 34, "Output",ExpressionUUID->"bd2ddcb6-e134-4d3d-8ec3-54a0ac4c9fac"],
Cell[10801, 268, 274, 3, 34, "Output",ExpressionUUID->"40260aa7-1d11-4cf7-b643-481aba628c78"]
}, Open  ]]
}
]
*)


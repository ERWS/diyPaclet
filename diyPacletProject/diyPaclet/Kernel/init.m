
mysetLocation[paclets : {___PacletManager`Pacletpaclets},location_String] := Map[mysetLocation[#, location] &, paclets];
mysetLocation[paclet_, location_String] := Function[
    	If[SameQ[PacletManager`Package`getPIValue[paclet, "Location"], 
      Null],
     		Append[paclet, "Location" -> #],
     		ReplaceAll[paclet, Rule["Location", _] :> ("Location" -> #)]]
    ][
   	If[Or[StringMatchQ[location, "http:*", IgnoreCase -> True],
     			StringMatchQ[location, "file:*", IgnoreCase -> True]
     (*Change1 Added https*)
               , StringMatchQ[location, "https:*", IgnoreCase -> True]
     		],
    		location, ExpandFileName@location
    	]
   ];

mydownloadpaclet[remotePaclet_PacletManager`Paclet, 
   Optional[async : True | False, False], OptionsPattern[]] := Module[
   	{loc,(* downloadTask,*) pacletFileName, 
    downloadedFileName,(*Change 2 for GitHub as Paclet is not in \
subdirectory*)$PacletSitePacletSubDirectory=""},
   	loc = PacletManager`Package`PgetLocation@remotePaclet;
   	pacletFileName = 
    StringJoin[
     PacletManager`Package`PgetQualifiedName@remotePaclet, 
     ".paclet"];
   	downloadedFileName = 
    ToFileName[PacletManager`Package`$userTemporaryDir,
     		StringJoin[
      PacletManager`Package`PgetQualifiedName@remotePaclet,
      			ToString@$ProcessID,
      			ToString@Random[Integer, {1, 1000}],
      			".paclet"]];
   	If[
    		Or[StringMatchQ[loc, "http:*", IgnoreCase -> True],
     			StringMatchQ[loc, "file:*", IgnoreCase -> True],
     (* Change 1 again Added https*)
     StringMatchQ[loc, "https:*", IgnoreCase -> True]],
    		If[async,(*Code removed but I could have  it in. 
     This code is not using an asynchronous task so  $Failed is \
returned instead*)
     			$Failed;
     			,
     			URLSave[
      StringJoin[loc,(*Change1*)$PacletSitePacletSubDirectory, 
       pacletFileName],
      				
      ToFileName[PacletManager`Package`$userTemporaryDir, 
       pacletFileName],
      				"UserAgent" -> PacletManager`Package`$userAgent, 
      BinaryFormat -> True
      			]
     		],
    		$Failed
    	]
   ];

Echo[#,"AddingSite"]&@PacletSiteAdd["https://github.com/ERWS/diyPaclet/releases/download/v1.0/", "ERWS", "Local" -> False];

Block[{PacletManager`Package`setLocation = mysetLocation,
  PacletManager`Package`downloadPaclet = mydownloadpaclet}, 
Echo[#,"CheckingPaclet"]&@PacletManager`Package`getPacletWithProgress["diyPaclet"]];

Get["diyPaclet`Kernel`diyPaclet`"]


(* Wolfram Language package *)
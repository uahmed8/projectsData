Porject := RTree Visualization or output in Latex file


Requirements:

GHC
Packages:
-threepenny-gui


How to run the program?
Open cmd and execute the following commands:
> cabal update
> cabal install threepenny-gui

run project file <.hs>: 
 There three parts that are being use somehow in file.
	1- Datastructure part that is completed.
		insert function can be checked by inserting MBBs (will always insert into leafnode) in RTree using below command:
			insertrt ((MBB 2.2 3.3 4.4 5.5),"Elem") emptyLeaf
		delete function can be checked by using below command:
			del "Elem" <current rtree in which MBB Elemm has been stored>
	2- GUI Part (So time consuming and difficult while using functions at backhand).
		This part is just showing the front hand as it was discussed. But inserting MBBs values and then clicking generate button is not working.
		 
		running part of GUI:
			command : main 
			an http will occur copy and and paste in google bar will show up the front hand part.
	3- Latex File part <This part is incomplete not working for children display>.
		console based there are some instruction could be followed while running below command:
			command: console
	  Checking tree generation in latex file: 
		command: l1 =  insertrt ((MBB 2.2 3.3 4.4 5.5),"Elem") emptyLeaf
		command: funcOutput l1
		a .tex file will generat in the specified path.

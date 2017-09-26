% All the predicates below are dynamic. In other words, are updated during the runtime using assert/1 or retract/1.
:- dynamic region/2.
:- dynamic hours/3.
:- dynamic capacity/2.
:- dynamic location/3.
:- dynamic limit/5.
:- dynamic lastpersonID/1.
:- dynamic hourofperson/3.
:- dynamic minuteofperson/3.
:- dynamic numberofpeopleofperson/3.
:- dynamic xcoordinateofperson/3.
:- dynamic ycoordinateofperson/3.
:- dynamic xdestinationofperson/3.
:- dynamic ydestinationofperson/3.
:- dynamic words/3.
:- dynamic location/3.
:- dynamic assigning/4.
:- dynamic rateoftravel/1.
:- dynamic printed/2.
:- dynamic phrase1/1.
:- dynamic phrase2/1.
:- dynamic words/3.


% Initializes the PersonID. Every request will be saved in the database with this unique identifier.
lastpersonID(0).


% Automatically starts the program
:- initialization(initiate).


% Function called to initialize the program.
initiate:-
	nl,
	writeln('Make sure the database file is in the same folder as the program, and both are located in the SWI-Prolog directory. Type the name of the file followed by .txt, and between apostrophes. Write "." then press ENTER:'),
	read(File),		% Reads the name of the file typed by the user.
	read_lines(File),	% Gives the name of the file as an argument to the function read_lines, that will open the .txt, get the necessary info, save it in the database, the close the file.
	deleteDirtyRegions,	
	nl,
	print('The database was succesfully loaded.'),
	nl,
	nl,
	write('Ready to accept requests. '),
	write('Please follow the instructions below:'),
	nl,
	nl,
	write('    A) Type the request between apostrophes'),
	nl,
	write('    B) Do not use any dots in between the apostrophes. Only exception is for writing decimal numbers'),
	nl,
	write('    C) Make sure all coordinates are decimals. If any coordinate is an integer, add a ".0" (zero) in the end of it'),
	nl,
	write('    D) Make sure that no coordinates are exactly the same. If two coordinates match, add a "0" (zero) in the end of one of them'),
	nl,
	write('    E) Make sure time is in the form XX:XX and in 24 hour time'),
	nl,
	write('    F) The request should contain the time, number of people, location and destination of the party'),
	nl,
	write('    G) When you are done, write "." then press ENTER'),
	nl,
	nl,
	write('For example:'),
	nl,
	nl,
	write('    pickup a party of 7 named Fox at 14:37 at location 43.6491340, -79.417345 and drop at 43.6559134, -54.453'),
	nl,
	nl,
	acceptrequests.		% Calls function acceptrequests


% If unable to load database, report problem.
initiate:-
	nl,
	print('Problem loading the database. Try again.'),
	nl.


% Function called to process request after loading the database.
acceptrequests :-
		nl,
		write('____________________________________________________'),
		nl,
		nl,
		writeln('Type the request:'),
		read(Request),						% Reads the request.
		nl,
		writeln('Type the rate of travel (minute/GPS) between apostrophes. Write "." then press ENTER:'),
		read(RateOfTravel1),					% Reads the rate of travel, which is a string.
		atom_number(RateOfTravel1, RateOfTravel2),		% Converts the string to a float.
		lastpersonID(LastPersonID),				% Looks in the database for the last PersonID used.
		NextPersonID is LastPersonID + 1,			% The next PersonID will be the last one, plus one.
		assigntaxi(Request,NextPersonID,RateOfTravel2),		% Calls function assigntaxi, that will parse the sentence and assign a taxi.
		retractall(lastpersonID(_)),				% Updates the last PersonID.
		assert(lastpersonID(NextPersonID)),
		oneMoreRequest.						% Calls itself (recursion), so when you're done with the request, it asks for another.


% If unable to load taxi, report problem, update PersonID and ask for another request.
acceptrequests :- 
		nl,
		nl,
		print('No taxi available for this request.'),
		nl,
		nl,
		nl,
		nl,
		lastpersonID(LastPersonID),
		NextPersonID is LastPersonID + 1,
		retractall(lastpersonID(_)),
		assert(lastpersonID(NextPersonID)),
		oneMoreRequest.


% After handling the last request, Prolog will ask if user wants to input another one
oneMoreRequest:-
		writeln('One more request? Type Y or N, inside apostrophes. Beware that typing N will shut down the program. Follow it by a "." then press ENTER:'),
		read(Answer),
		decideOnAnswer(Answer).


% If yes, call predicate acceptrequests/0
decideOnAnswer(Answer):-
		Answer = 'Y',
		acceptrequests.
		

% If no, close program		
decideOnAnswer(Answer):-
		Answer = 'N',
		halt.


% If unable to understand answer, call oneMoreRequest/0 again
decideOnAnswer(_):- oneMoreRequest.


% Opens file, then calls function to read content, then close file (seen).
read_lines(File) :- 
		see(File), 
		readthe(File),		% Calls function readthe, that will read the file line by line.
		seen.
		
		
read_lines(File) :- 
		see(File), 
		readthe2(File), 
		cleanRegions([]),
		seen.
		
		
read_lines(File) :- 
		see(File), 
		readthe2(File), 
		seen.


% Reads the file line by line.
readthe(File):-
	read_one_line(Codes), 				% Gets next unread line as a list of codes
	atom_codes(Answer, Codes),			% Transforms the list of codes into a string. Example: 'T11, R1, 6:00-18:00, 6, 43.668943, -79.386211'
	atomic_list_concat(Phrase,',', Answer),		% Breaks string into a list. Example: ['T11',' R1',' 6:00-18:00',' 6',' 43.668943',' -79.386211']
	break(Phrase),					% Calls function break, that will take each element and save it appropriately in the database
	readthe(File).
	
	
readthe2(File):-
	read_one_line2(Codes), 
	atom_codes(Answer, Codes),
	atomic_list_concat(Phrase,',', Answer),
	[_|_] = Phrase,
	break(Phrase),
	readthe2(File).


readthe2(_).


% Taxi Info: Takes each element of a line from the .txt file and saves it appropriately in the database
break(Phrase):-
    [Taxi,X2,X3,X4,X5,X6|_] = Phrase,				% Separates the list into many parts
    atomic_list_concat(RegionList,' ', X2),			
    [_|Region1] = RegionList,
    [Region2|_] = Region1,					% Gets the region of the taxi. And so on for every necessary information.
    atomic_list_concat(HourList1,' ', X3),
    [_,HourList2|_] = HourList1,				
    atomic_list_concat(HourList3,'-', HourList2),
    [Beginning1,End1|_] = HourList3,
    atomic_list_concat(Beginning2,':', Beginning1),
    atomic_list_concat(End2,':', End1),
    [BeginningHour1,BeginningMinute1|_] = Beginning2,
    atom_number(BeginningHour1, BeginningHour2),
    atom_number(BeginningMinute1, BeginningMinute2),
    Time1 = time(BeginningHour2,BeginningMinute2),
    [EndHour1,EndMinute1|_] = End2,
    atom_number(EndHour1, EndHour2),
    atom_number(EndMinute1, EndMinute2),
    Time2 = time(EndHour2,EndMinute2),
    atomic_list_concat(Capacity1,' ', X4),
    [_,Capacity2|_] = Capacity1,
    atom_number(Capacity2, Capacity3),
    atomic_list_concat(XCoordinate1,' ', X5),
    [_,XCoordinate2|_] = XCoordinate1,
    atom_number(XCoordinate2, XCoordinate3),
    atomic_list_concat(YCoordinate1,' ', X6),
    [_,YCoordinate2|_] = YCoordinate1,
    atom_number(YCoordinate2, YCoordinate3),
    assert(region(Taxi,Region2)),				% Saves the region of the taxi in the database.
    assert(hours(Taxi,Time1,Time2)),				% Saves the hours of operation of the taxi.
    assert(capacity(Taxi,Capacity3)),				% Saves the capacity of the taxi.
    assert(location(Taxi,XCoordinate3,YCoordinate3)).		% Saves initial location of the taxi.


% Regions Info: Takes each element of a line from the .txt file and saves it appropriately in the database
break(Phrase):-
    [Region,X1,X2,X3,X4|_] = Phrase,
    assert(limit(Region,X1,X2,X3,X4)).


% Gets the next line of the .txt file. If there are no more lines to read, predicate fails and text file is closed.
read_one_line(Codes) :- 
    get0(Code), 
    (   Code < 0 /* end of file */ -> 
        Codes = [] 
    ;   Code =:= 10 /* end of line */ -> 
        Codes = [] 
    ;   Codes = [Code|Codes1], 
        read_one_line(Codes1) 
    ). 


read_one_line2(Codes) :- 
    get(Code), 
    (   Code < 0 /* end of file */ -> 
        Codes = [] 
    ;   Code =:= 10 /* end of line */ -> 
        Codes = [] 
    ;   Codes = [Code|Codes1], 
        read_one_line(Codes1) 
    ). 
   
   
% For every region saved in the database, it converts both coordinates from strings to floats, and saves it in the database
cleanRegions(List):-
	region(_,Region),
	not(member(Region,List)),
	limit(Region,X1,X2,X3,X4),
	atomic_list_concat(X12,' ', X1),
	atomic_list_concat(X22,' ', X2),
	atomic_list_concat(X32,' ', X3),
	atomic_list_concat(X42,' ', X4),
	[_,X13|_] = X12,
	[_,X23|_] = X22,
	[_,X33|_] = X32,
	[_,X43|_] = X42,
	atom_number(X13, X14),
	atom_number(X23, X24),
	atom_number(X33, X34),
	atom_number(X43, X44),
	assert(limit(Region,X14,X24,X34,X44)),
	append(Region,List,List2),
	cleanRegions(List2).


% Since the function defined above tries to read every line of the .txt and save it as a region, you have to delete entries in the database that do not make sense
deleteDirtyRegions:-
	limit(Region,X1,X2,X3,X4),
	(
	not(float(X1));
	not(float(X2));
	not(float(X3));
	not(float(X4))
	),
	retract(limit(Region,X1,X2,X3,X4)),
	deleteDirtyRegions.
	
	
deleteDirtyRegions.


% Defines function to append an element to a list.
append([],L,L). 
append([H|T],L2,[H|L3])  :-  append(T,L2,L3).


% Defines function to see if an element is a member of a list.
member(X,[X|_]). 
member(X,[_|T])  :-  member(X,T).


% Function that gets a parsed request (['pickup','a','party','of','4',...]) and extracts the necessary information
understand(PersonID):-
		gethour(PersonID),				% Gets the hour person wants to be picked up
		getminute(PersonID),				% Gets the minute person wants to be picked up
		getnumberofpeople(PersonID),			% And so on
		getXcoordinate(PersonID),
		getYcoordinate(PersonID),
		getXdestination(PersonID),
		getYdestination(PersonID),
		hourofperson(PersonID,_,Word1),			% After extracting all the info and saving in the database, it prints it to the user
		minuteofperson(PersonID,_,Word2),		
		numberofpeopleofperson(PersonID,_,Word3),
		xcoordinateofperson(PersonID,_,Word4),
		ycoordinateofperson(PersonID,_,Word5),
		xdestinationofperson(PersonID,_,Word6),
		ydestinationofperson(PersonID,_,Word7),
		nl,
		print('Hour: '),
		print(Word1),
		nl,
		print('Minute: '),
		print(Word2),
		nl,
		print('Number of People: '),
		print(Word3),
		nl,
		print('X Coordinate: '),
		print(Word4),
		nl,
		print('Y Coordinate: '),
		print(Word5),
		nl,
		print('X Destination: '),
		print(Word6),
		nl,
		print('Y Destination: '),
		print(Word7).


% Same function as above, though now the program searches for the coordinates of the destination before finding the initial location.
% This is important because you can define the coordinates of the initial location as not coordinates of destination, and vice-versa.
% So you only need to find one or the other using the method presented in a bit.
understand(PersonID):-
		gethour(PersonID),
		getminute(PersonID),
		getnumberofpeople(PersonID),
		getXdestination(PersonID),
		getYdestination(PersonID),
		getXcoordinate(PersonID),
		getYcoordinate(PersonID),
		hourofperson(PersonID,_,Word1),
		minuteofperson(PersonID,_,Word2),
		numberofpeopleofperson(PersonID,_,Word3),
		xcoordinateofperson(PersonID,_,Word4),
		ycoordinateofperson(PersonID,_,Word5),
		xdestinationofperson(PersonID,_,Word6),
		ydestinationofperson(PersonID,_,Word7),
		print('Hour: '),
		print(Word1),
		nl,
		print('Minute: '),
		print(Word2),
		nl,
		print('Number of People: '),
		print(Word3),
		nl,
		print('X Coordinate: '),
		print(Word4),
		nl,
		print('Y Coordinate: '),
		print(Word5),
		nl,
		print('X Destination: '),
		print(Word6),
		nl,
		print('Y Destination: '),
		print(Word7).


% The hour that the person wants to be picked up will be an integer followed by an integer
gethour(PersonID):-
		words(PersonID,Location,Word1),
		integer(Word1),
		NextLocation is Location + 1,
		words(PersonID,NextLocation,Word2),
		integer(Word2),
		assert(hourofperson(PersonID,Location,Word1)).


% The minute that the person wants to be picked up will be an integer preceded by an integer
getminute(PersonID):-
		words(PersonID,Location,Word1),
		integer(Word1),
		FormerLocation is Location - 1,
		words(PersonID,FormerLocation,Word2),
		hourofperson(PersonID,FormerLocation,Word2),
		assert(minuteofperson(PersonID,Location,Word1)).


% The number of people will be an integer that is not the hour nor the minute that the person wants to be picked up
getnumberofpeople(PersonID):-
		words(PersonID,Location1,Word1),
		integer(Word1),
		hourofperson(PersonID,Location2,_),
		minuteofperson(PersonID,Location3,_),
		Location1 =\= Location2,
		Location1 =\= Location3,
		assert(numberofpeopleofperson(PersonID,Location1,Word1)).


% The X coordinate of the initial location of the person will be a float, such that you can find a word or pair of words before it
% that indicates an "initial state", and there's no floats in between.
% For example, "we are", "get us", "am located", "pickup".
getXcoordinate(PersonID):-
			words(PersonID,Location1,Word1),
			float(Word1),
			words(PersonID,Location2,Word2),
			Location2 < Location1,
			Location4 is Location2 - 1,
			words(PersonID,Location4,Word4),
			indicatesinitialstate(Word2,Word4),
			not(floatinbetween(PersonID,Location1,Location2)),
			assert(xcoordinateofperson(PersonID,Location1,Word1)).


% Or the X coordinate of the initial location of the person will be a float, that is not the X coordinate of the destination, and it's preceded by a string 
getXcoordinate(PersonID):-
			xdestinationofperson(PersonID,Location1,_),
			words(PersonID,Location2,Word2),
			Location1 =\= Location2,
			float(Word2),
			Location3 is Location2 - 1,
			words(PersonID,Location3,Word3),
			atom(Word3),			
			assert(xcoordinateofperson(PersonID,Location2,Word2)).


% The Y coordinate of the initial location is a float that comes right after the X coordinate
getYcoordinate(PersonID):-
		words(PersonID,Location,Word1),
		float(Word1),
		FormerLocation is Location - 1,
		words(PersonID,FormerLocation,Word2),
		xcoordinateofperson(PersonID,FormerLocation,Word2),
		assert(ycoordinateofperson(PersonID,Location,Word1)).


getYcoordinate(PersonID):-
		words(PersonID,Location,Word1),
		float(Word1),
		FormerLocation is Location - 2,
		words(PersonID,FormerLocation,Word2),
		xcoordinateofperson(PersonID,FormerLocation,Word2),
		assert(ycoordinateofperson(PersonID,Location,Word1)).


% The X coordinate of the destination of the person will be a float, such that you can find a word or pair of words before it
% that indicate a "final state", and there's no floats in between.
% For example, "leave us", "destination", "take us".
getXdestination(PersonID):-
			words(PersonID,Location1,Word1),
			float(Word1),
			words(PersonID,Location2,Word2),
			Location2 < Location1,
			Location4 is Location2 - 1,
			words(PersonID,Location4,Word4),
			indicatesfinalstate(Word2,Word4),
			not(floatinbetween(PersonID,Location1,Location2)),
			assert(xdestinationofperson(PersonID,Location1,Word1)).


% Or the X coordinate of the destination of the person will be a float, that is not the X coordinate of the initial location, and it's preceded by a string 
getXdestination(PersonID):-
			xcoordinateofperson(PersonID,Location1,_),
			words(PersonID,Location2,Word2),
			Location1 =\= Location2,
			float(Word2),
			Location3 is Location2 - 1,
			words(PersonID,Location3,Word3),
			atom(Word3),			
			assert(xdestinationofperson(PersonID,Location2,Word2)).


% The Y coordinate of the destination is a float that comes right after the X coordinate
getYdestination(PersonID):-
		words(PersonID,Location,Word1),
		float(Word1),
		FormerLocation is Location - 1,
		words(PersonID,FormerLocation,Word2),
		xdestinationofperson(PersonID,FormerLocation,Word2),
		assert(ydestinationofperson(PersonID,Location,Word1)).
	
	
getYdestination(PersonID):-
		words(PersonID,Location,Word1),
		float(Word1),
		FormerLocation is Location - 2,
		words(PersonID,FormerLocation,Word2),
		xdestinationofperson(PersonID,FormerLocation,Word2),
		assert(ydestinationofperson(PersonID,Location,Word1)).


% Word or pair of words that may indicate that you're saying where you are ot where you wanna be picked up
indicatesinitialstate(Word2, Word1):-
			Word2 = 'pickup';
			Word1 = 'pick',
			Word2 = 'us';
			Word1 = 'pick',
			Word2 = 'me';
			Word2 = 'picked';
			Word1 = 'we',
			Word2 = 'are';
			Word1 = 'am',
			Word2 = 'located';
			Word1 = 'are',
			Word2 = 'located';
			Word1 = 'get',
			Word2 = 'us';
			Word1 = 'calling',
			Word2 = 'from';
			Word1 = 'send',
			Word2 = 'cab';
			Word1 = 'send',
			Word2 = 'taxi';
			Word1 = 'get',
			Word2 = 'me';
			Word2 = 'send'.


% Word or pair of words that may indicate where you wanna go
indicatesfinalstate(Word2, Word1):-
			Word2 = 'drop';
			Word1 = 'drop',
			Word2 = 'us';
			Word1 = 'drop',
			Word2 = 'off';
			Word2 = 'leave';
			Word2 = 'left';
			Word1 = 'leave',
			Word2 = 'us';
			Word1 = 'leave',
			Word2 = 'me';
			Word2 = 'destination';
			Word2 = 'dropped';
			Word2 = 'take';
			Word1 = 'take',
			Word2 = 'us';
			Word1 = 'must',
			Word2 = 'be';
			Word1 = 'take',
			Word2 = 'me';
			Word2 = 'go'.


% Function to see if there's another float between the float in question and the pair of words
floatinbetween(PersonID,Location1,Location2):-
					words(PersonID,Location3,Word3),
					Location3 < Location1,
					Location2 < Location3,
					float(Word3).


% Function called by acceptrequests to parse the request, then assign a taxi
assigntaxi(Statement,PersonID,RateOfTravel):-
				request(Statement, PersonID),				% Function request parses the request
				retractall(rateoftravel(_)),
				assert(rateoftravel(RateOfTravel)),			% Updates the rate of travel as assigned by the user
				numberofpeople(PersonID,Number),
				!,							% Cut prevents prolog to try and "parse" request in a different way
				assign(_,PersonID,numberofpeople(PersonID,Number),1),	% Calls function to assign taxi
				nl,
				nl,
				printassignments(PersonID),				% Calls function to print assignments to user
				nl,
				nl.


% For every assignment of taxi to user - there can be more than one - it prints the assignment to the user and the time for the taxi to arrive
printassignments(PersonID):-
				assigning(Taxi,PersonID,_,TimeToPassenger),
				not(alreadyprinted(Taxi,PersonID)),
				print('Assign: '),
				print(Taxi),
				nl,
				print('Time to get to passenger (minutes): '),
				print(TimeToPassenger),
				nl,
				nl,
				assert(printed(Taxi,PersonID)),
				printassignments(PersonID).
	
	
printassignments(_).


alreadyprinted(Taxi,PersonID):-
				printed(Taxi,PersonID).


% Function to parse request
request(Statement, PersonID) :-
				atomic_list_concat(List,' ',Statement),		% Breaks request into a list of strings, considering blank spaces " "
				break(List,[]),					% Breaks list into another list of strings, considering ":"
				phrase1(X),
				break2(X,[]),					% Breaks list into another list of strings, considering ","
				phrase2(Y),
				break3(Y,[]),					% Breaks list into another list of strings, considering "?"
				phrase3(K),
				break5(K,[]),					% Breaks list into another list of strings, considering "("
				phrase5(J),
				break6(J,[]),					% Breaks list into another list of strings, considering ")". Output is a "clean" list.
				phrase6(L),
				assert(words(PersonID, 0, ' ')),
				savephrase(L,1,PersonID),			% Saves every word in the database and its respective location in the sentence
				!,
				understand(PersonID),				% Calls function understand, to take the meaning out of the words
				hourofperson(PersonID,_,Word1),
				minuteofperson(PersonID,_,Word2),
				numberofpeopleofperson(PersonID,_,Word3),
				xcoordinateofperson(PersonID,_,Word4),
				ycoordinateofperson(PersonID,_,Word5),
				xdestinationofperson(PersonID,_,Word6),
				ydestinationofperson(PersonID,_,Word7),
				assert(calledat(PersonID,time(Word1,Word2))),	% Gets output of understand, and saves it in the database in the format needed for function assign
				assert(coordinates(PersonID,Word4,Word5)),
				assert(numberofpeople(PersonID,Word3)),
				assert(destination(PersonID,Word6,Word7)).


% Breaks list into another list of strings, considering ":"
break(List1,TempList):-
		[H|_] = List1,
		atomic_list_concat(Clean,':',H),
		append(TempList,Clean,FinalList),
		delete(List1, H, List2),
		retractall(phrase1(_)),
		assert(phrase1(FinalList)),
		break(List2,FinalList).
	
	
break(_,_).


% Breaks list into another list of strings, considering ","
break2(List1,TempList):-
		[H|_] = List1,
		atomic_list_concat(Clean,',',H),
		append(TempList,Clean,FinalList),
		delete(List1, H, List2),
		retractall(phrase2(_)),
		assert(phrase2(FinalList)),
		break2(List2,FinalList).


break2(_,_).


% Breaks list into another list of strings, considering "?"
break3(List1,TempList):-
		[H|_] = List1,
		atomic_list_concat(Clean,'?',H),
		append(TempList,Clean,FinalList),
		delete(List1, H, List2),
		retractall(phrase3(_)),
		assert(phrase3(FinalList)),
		break3(List2,FinalList).


break3(_,_).


% Breaks list into another list of strings, considering "("
break5(List1,TempList):-
		[H|_] = List1,
		atomic_list_concat(Clean,'(',H),
		append(TempList,Clean,FinalList),
		delete(List1, H, List2),
		retractall(phrase5(_)),
		assert(phrase5(FinalList)),
		break5(List2,FinalList).


break5(_,_).


% Breaks list into another list of strings, considering ")"
break6(List1,TempList):-
		[H|_] = List1,
		atomic_list_concat(Clean,')',H),
		append(TempList,Clean,FinalList),
		delete(List1, H, List2),
		retractall(phrase6(_)),
		assert(phrase6(FinalList)),
		break6(List2,FinalList).


break6(_,_).


% Saves every word of the sentence in the database, with its respective location. Also, when you can convert the string to a number, you do it.
savephrase(List1,Location,PersonID):-
				[H|_] = List1,
				atom_number(H, H1),
				assert(words(PersonID, Location, H1)),
				delete(List1, H, List2),
				NextLocation is Location + 1,
				savephrase(List2,NextLocation,PersonID).


savephrase(List1,Location,PersonID):-
				[H|_] = List1,
				assert(words(PersonID, Location, H)),
				delete(List1, H, List2),
				NextLocation is Location + 1,
				savephrase(List2,NextLocation,PersonID).


savephrase(_,_,_).


% Function that assigns a taxi to a person - Basically, Part II of the project
assign(Taxi,PersonID,numberofpeople(PersonID,Quantity),FormerTaxi):-
						region(Taxi,Area),						% Gets area that the taxi serves
						islocated(PersonID,Area),					% Calls function to see if person is located in that area
						hours(Taxi,time(X1,X2),time(X3,X4)),				% Gets time that the taxi is functioning
						calledat(PersonID,time(X5,X6)),					% Gets time that the person called
						timeframe(time(X1,X2),time(X3,X4),time(X5,X6)),			% Calls function to see if the time that the person called is inside the time range of the taxi
						location(Taxi,R,T),						% Gets initial location of the taxi
						destination(PersonID,S,Q),					% Gets destination of the person
						coordinates(PersonID, Z, K),					% Gets location of the person
						Distance1 is sqrt((R - Z)*(R - Z) + (T - K)*(T - K)),		% Calculates distance from initial location of taxi to location of person
						Distance2 is sqrt((Z - S)*(Z - S) + (K - Q)*(K - Q)),		% Calculates distance from location of person to destination
						Distance3 is sqrt((S - R)*(S - R) + (Q - T)*(Q - T)),		% Calculates distance from destination to initial location of taxi
						rateoftravel(N),						% Gets rate of travel assigned by the user
						Ridetime is ceiling((Distance1 + Distance2 + Distance3)*N),	% Calculates total time of the ride
						not(isnotfree(Taxi,PersonID,Ridetime)),				% Checks if taxi is available given the time and time of the ride
						capacity(Taxi, G),						% Checks if taxi capacity is enough for number of people	
						Quantity =< G,
						not(Taxi = FormerTaxi),						% If you're assigning more than one taxi per request, makes sure you are assigning different taxis
						TimeToLocation is ceiling(Distance2*N),
						assert(assigning(Taxi,PersonID,Ridetime,TimeToLocation)).	% Saves assigning of taxi to person in the database


% If no available taxi can, by itself, drive everybody in the party, it's necessary to assign more than one taxi
assign(Taxi,PersonID,numberofpeople(PersonID,Quantity),FormerTaxi):-
						region(Taxi,Area),
						islocated(PersonID,Area),
						hours(Taxi,time(X1,X2),time(X3,X4)),
						calledat(PersonID,time(X5,X6)),
						timeframe(time(X1,X2),time(X3,X4),time(X5,X6)),
						destination(PersonID,S,Q),
						location(Taxi,R,T),
						coordinates(PersonID, Z, K),
						Distance1 is sqrt((R - Z)*(R - Z) + (T - K)*(T - K)),
						Distance2 is sqrt((Z - S)*(Z - S) + (K - Q)*(K - Q)),
						Distance3 is sqrt((S - R)*(S - R) + (Q - T)*(Q - T)),
						rateoftravel(N),
						Ridetime is ceiling((Distance1 + Distance2 + Distance3)*N),
						not(isnotfree(Taxi,PersonID,Ridetime)),
						capacity(Taxi,G),
						Quantity > G,							% Finds a taxi that respect all the constraints, except enough capacity													
						Peopleleft is Quantity - G,
						not(Taxi = FormerTaxi),
						assign(_,PersonID,numberofpeople(PersonID,Peopleleft),Taxi),	% Then assuming that it assigned this taxi to the person, tries to assign another taxi to take care of the left quantity of people using recursion, until the capacity of all assigned taxis is greater or equal
						TimeToLocation is ceiling(Distance2*N),
						assert(assigning(Taxi,PersonID,Ridetime,TimeToLocation)).	% Saves assigning in the database only after making sure there are enough taxis available to take care of demand


% Function that gets two times and checks if one is earlier than the other
earlier(time(H,M1),time(H,M2)) :- !, M1 < M2.
earlier(time(H1,_),time(H2,_)) :- H1 < H2.


% Function to check if time belongs to time range, when time range starts and ends in the same day. Example: 6:00 to 18:00
timeframe(time(X1,X2),time(X3,X4),time(X5,X6)):-
					earlier(time(X1,X2),time(X3,X4)),
					earlier(time(X1,X2),time(X5,X6)),
					earlier(time(X5,X6),time(X3,X4)).


% Function to check if time belongs to time range, when time range starts in one day and ends in the other. Example: 18:00 to 6:00
timeframe(time(X1,X2),time(X3,X4),time(X5,X6)):-
					not(earlier(time(X1,X2),time(X3,X4))),
					earlier(time(X1,X2),time(X5,X6));
					earlier(time(X5,X6),time(X3,X4)).


% Function to check if person is located in certain region.
% There are four functions, since a rectangle can be defined by two points, in two different orders. Example: down left corner then upper right corner, or the opposite order. Or upper left corner, and down right corner, or the opposite order.
islocated(Person,Area):-
		limit(Area,A,B,C,D),
		coordinates(Person,Z,K),
		A < C,
		B < D,
		A =< Z,
		Z =< C,
		B =< K,
		K =< D.


islocated(Person,Area):-
		limit(Area,A,B,C,D),
		coordinates(Person,Z,K),
		A > C,
		B < D,
		C =< Z,
		Z =< A,
		B =< K,
		K =< D.


islocated(Person,Area):-
		limit(Area,A,B,C,D),
		coordinates(Person,Z,K),
		A < C,
		B > D,
		A =< Z,
		Z =< C,
		D =< K,
		K =< B.


islocated(Person,Area):-
		limit(Area,A,B,C,D),
		coordinates(Person,Z,K),
		A > C,
		B > D,
		C =< Z,
		Z =< A,
		D =< K,
		K =< B.


% Function to add minutes to a time. For example, ride starts at 21:30 and takes 50 minutes. It ends at 22:20.
addtime(time(X1,X2),Minutes,X3,X4):-
				Total1 is X1*60 + X2,
				Total2 is Total1 + Minutes,
				Total2 < 1440,
				Total3 is Total2 - mod(X2 + Minutes,60),
				X3 is Total3/60,
				X4 is mod(X2 + Minutes,60).


% Same function, but for a ride that starts before midnight and ends after it
addtime(time(X1,X2),Minutes,X3,X4):-
				Total1 is X1*60 + X2,
				Total2 is Total1 + Minutes,
				%Total2 >= 1440,
				Total3 is Total2 - 1440 - mod(X2 + Minutes,60),
				X3 is Total3/60,
				X4 is mod(X2 + Minutes,60).


% Function to check whether taxi is available, given start time and duration of ride.
isnotfree(Taxi,PersonID1,Ridetime1):-
				assigning(Taxi,PersonID2,Ridetime2,_),	% It is not available if you can find a previus assignment, such that
				calledat(PersonID1,time(X1,X2)),	
				calledat(PersonID2,time(X3,X4)),	
				addtime(time(X3,X4),Ridetime2,X5,X6),
				addtime(time(X1,X2),Ridetime1,X7,X8),
				earlier(time(X1,X2),time(X5,X6)),	% it ends after the new ride starts
				earlier(time(X3,X4),time(X7,X8)).	% and it starts before the new activity ends.


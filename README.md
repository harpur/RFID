# RFID

This is an upcoming R package that will deal with MajaIV V4.2.0.0 RFID data. As of right now, it takes in a raw xlsx output and mungs it into a more useable format

Input File:
<pre><code>
UTCTime_Creation	Company	Program_Version	UTCTime	ReaderID	Address	UID	ScanCount	Type
9/27/2016 3:30:10 PM	MICROSENSYS	MajaIV, Version=4.2.0.0, Culture=neutral-4.2.0.0	09/27/2016 15:30:08.981	49702	1	15 C6 4F 05 09 00 12 E0	1	177
9/27/2016 3:30:10 PM	MICROSENSYS	MajaIV, Version=4.2.0.0, Culture=neutral-4.2.0.0	09/27/2016 15:33:21.339	49702	1	11 B8 66 01 0B 00 12 E0	1	177
9/27/2016 3:30:10 PM	MICROSENSYS	MajaIV, Version=4.2.0.0, Culture=neutral-4.2.0.0	09/27/2016 15:34:50.449	9519	5	15 C6 4F 05 09 00 12 E0	1	177
9/27/2016 3:30:10 PM	MICROSENSYS	MajaIV, Version=4.2.0.0, Culture=neutral-4.2.0.0	09/27/2016 15:34:50.681	9519	5	15 C6 4F 05 09 00 12 E0	1	177
</code></pre>

Output file:
<pre><code>
         UTCTime_Round                     UID  timediff dir
107 2016-09-27 16:35:28 4D B7 66 01 0B 00 12 E0 0.2249999 5-5
108 2016-09-27 16:35:28 4D B7 66 01 0B 00 12 E0 4.1470001 5-8
109 2016-09-27 16:35:33 4D B7 66 01 0B 00 12 E0 8.4180000 8-8
110 2016-09-27 16:35:41 4D B7 66 01 0B 00 12 E0 3.2860000 8-5
111 2016-09-27 16:35:44 4D B7 66 01 0B 00 12 E0 0.0000000 5-0
</code></pre>

The output contains the time of the event, the bee's ID, the time (in seconds) spent on the event, and the direction of the event (which readers they hit in order)


The script requires 2 R packages: gdata and dataframe2xls

I recommend concatenating all output files together into a single xlsx file.

To run on a single input file: 
<pre><code>
Rscript MajaMung.r <INPUT.xlsx> <OUTPUT FILE PREFIX>
</code></pre>
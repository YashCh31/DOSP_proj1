Readme - Project 1


# FINDING BITCOIN

Bitcoins use the hardness of cryptographic hashing to ensure a limited “supply” of coins. The cryptographic algorithm for hashing used for this project is SHA-256.
We need to find hash values of random string and ensure that the number of leading zeros in the hashed value matches to the input number provided by the user. In case the input number and number of leading zeros in hash values is a match, we have found the bitcoin.

The goal of this project is to use Erlang and the actor model to build a good solution to this problem that runs well on multi-core machines.

- Requirements:
Input: The input provided (in command line to your bcms_s.erl) will be the required number of 0s of the bitcoin that needs to be mined.

- Output Expected : Print, on independent entry lines, the input string, and the corresponding SHA256 hash separated by a TAB, for each of the bitcoins we find. Obviously, our SHA256 hash must have the required number of leading 0s (k= 3 means3 0’s in the hash notation).  An extra requirement, to ensure every group finds different coins, is to have the input string prefixed by the gator link ID of one of the team members.


## Group Members
1. Divyajyoti Ukirde, UFID: 30260576, Email: divyajyotiukirde@ufl.edu
2. Yashwanth Venkat Chandolu, UFID: 11429359, Email: ychandolu@ufl.edu


## Project Description

The following section describes the various aspects of the program.

### File Descriptions
1. bcms_s.erl : This file represents the server.
2. bcms_c.erl : This file represents the client.

### Instruction Details
This project has been developed using VS Code/ Atom Text Editor.

Please make sure you have erlang is installed in the system.

Create two nodes using “erl -sname c” for client and “erl -sname s” for server.
Compile both the server and client files in the respective terminals.  Run the commands “c(bcms_c).” and “c(bcms_s).” to compile both the files.
You need to register the server and the client worker by running “bcms_s:reg()” and “bcms_c:reg()” in their respective terminals.
Execute the init function of the server by running “bcms_s:init().” Here, the terminal prompts the user to enter the number of 0s.
Connect the client to the server by typing “bcms_c:connect(‘Device ID of the server’).” This connects the client to the server. The client receives the job from the server starts mining once it is connected.


### Machine Details

We tested the code on two machines:
1. 8th Gen Intel 2.3 GHz Quad Core Processor
2. Intel(R) Core(TM) i5-7200U CPU @ 2.50GHz   2.70 GHz


### 1. Work Unit
1) Server: The work unit for every individual actor in the server is (36 ^ characterCount). We did this in order to ensure multiple permutations of of each character count.
2) Client: The work unit for every individual actor in the client is (36 ^ characterCount) % numberOfActors. We did this to ensure that all the threads on the machine are being utilised and reduction in the number of messages passing between parent and miners.

### 2. Results

Result of Running the program for finding hash starting with 5 0s with one client and one server.
CPU Time = 226000 Microseconds
Real Time = 94000 Microseconds
Ratio of CPU Utilisation: 2.404

Result of Running the program for finding hash starting with 6 0s with client spawning 3 workers and one server.
CPU Time = 678000 Microseconds
Real Time = 257000 Microseconds
Ratio of CPU Utilisation: 2.638

Result of Running the program for finding hash with 4 0s and client spawning 8 workers and one server.
CPU Time = 115000 Microseconds
Real Time = 39000 Microseconds
Ratio of CPU Utilisation = 2.948

When we try to run only the server for finding hash starting with 3 0s without connecting the client.

CPU Time = 120000 Microseconds
Real Time = 89000 Microseconds
Ratio of CPU Utilisation: 1.348


###3. The coin with most number of 0s that we managed to find is 7:

String - YashwanthChandolu:nDHqFgFFCYU
Hash Value - 0000000415dcf6df2084a91db33aba5872b2842cc5cced6e3df3cbc871242027


### 4. Largest number of working machines we were able to run the code with

We were able to run the code on two machines.

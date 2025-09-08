       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       AUTHOR. TEAM WYOMING.


       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT userInputFile ASSIGN TO "src/files/input.txt"
              ORGANIZATION IS LINE SEQUENTIAL. *>Records are stored one after another.
       SELECT userOutputFile ASSIGN TO "src/files/output.txt"
              ORGANIZATION IS LINE SEQUENTIAL.
       SELECT accountFile ASSIGN TO "src/files/account.txt"
                ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
           FD userInputFile.
           01 userInputRecord PIC X(100).
           FD userOutputFile.
           01 userOutputRecord PIC X(100).
           FD accountFile.
           01 accountRecord PIC X(100).

         WORKING-STORAGE SECTION.
           *>Variables for user input and output
           01 newUser        PIC X.
           01 userName       PIC X(30).
           01 userPassword   PIC X(30).
           01 messageVar     PIC X(100).

           *>Password validation variables
           01 passwordValid    PIC X VALUE "N".
           01 i                PIC 99 VALUE 1.
           01 upperFound       PIC X VALUE "N".
           01 digitFound       PIC X VALUE "N".
           01 specialFound     PIC X VALUE "N".
           01 char             PIC X.

           *>Account counting and verification variables
           01 accountCount    PIC 9 VALUE 0.
           01 endOfFile       PIC X VALUE "N".


       PROCEDURE DIVISION.
           OPEN INPUT userInputFile
           OPEN OUTPUT userOutputFile *>Overwrites existing output file , if any. Delete to append instead.
           READ userInputFile INTO newUser.
           READ userInputFile INTO userName.
           READ userInputFile INTO userPassword.


           PERFORM countAccounts.
           OPEN EXTEND accountFile *> appends instead of overwriting.

           Move "Welcome to inCollege by team Wyoming!" TO messageVar
           PERFORM displayAndWrite.
           MOVE "Are you a new user? (Y/N)" TO messageVar
           PERFORM displayAndWrite.


           DISPLAY accountCount " accounts currently exist.".*>Debug, can be removed
           IF newUser = "Y" OR newUser = "y"
               IF accountCount >= 5
                   DISPLAY "Maximum number of accounts reached. Cannot register new users."
                   CLOSE userInputFile
                   CLOSE userOutputFile
                   CLOSE accountFile
                   STOP RUN
               ELSE
                   PERFORM newUserRegistration
               END-IF
           ELSE
               PERFORM existingUserLogin
           END-IF

           DISPLAY "Thank you for using inCollege".
           CLOSE userInputFile.
           CLOSE userOutputFile.
           CLOSE accountFile.
           STOP RUN.

           *> *****************Subroutines to be called*****************

           *> New user registration process
           newUserRegistration.
               PERFORM validatePassword.
                   IF passwordValid = "Y"
                       MOVE userName TO accountRecord(1:30)
                       MOVE userPassword TO accountRecord(31:60)
                       WRITE accountRecord
                       DISPLAY "Account Created."

                   ELSE
                       MOVE "Please try again. Password must be 8 or 12 characters long." TO messageVar
                       PERFORM displayAndWrite.
                   END-IF.
               EXIT.

           *> Existing user login process (NEEDS TO BE IMPLEMENTED)
           existingUserLogin.
               MOVE "Please enter your login credentials." TO messageVar
               PERFORM displayAndWrite.
               MOVE "Login process completed." TO messageVar
               PERFORM displayAndWrite.
               EXIT.


           *> Validate password length
           validatePassword.
                IF FUNCTION LENGTH(FUNCTION TRIM(userPassword TRAILING))  > 8 AND FUNCTION LENGTH(FUNCTION TRIM(userPassword TRAILING)) < 12
                    MOVE "Y" TO passwordValid
                ELSE
                    MOVE "Invalid password length: " FUNCTION LENGTH(FUNCTION TRIM(userPassword TRAILING)) ". Password must be between 8 and 12 characters." TO messageVar
                    PERFORM displayAndWrite
                END-IF.

               PERFORM Varying i FROM 1 BY 1 UNTIL i > FUNCTION LENGTH(FUNCTION TRIM(userPassword TRAILING))
                   MOVE userPassword(i:1) TO char
                   IF char >= "A" AND char <= "Z"
                       MOVE "Y" TO upperFound
                   END-IF
                   IF char >= "0" AND char <= "9"
                       MOVE "Y" TO digitFound
                   END-IF
                   IF char = "!" OR char = "@" OR char = "#" OR char = "$" OR char = "%" OR char = "^" OR char = "&" OR char = "*"
                       MOVE "Y" TO specialFound
                   END-IF
                END-PERFORM.
                   IF upperFound = "Y" AND digitFound = "Y" AND specialFound = "Y"
                          MOVE "Y" TO passwordValid
                   ELSE
                       MOVE "Password must contain at least one uppercase letter, one special character, and one digit." TO messageVar
                       PERFORM displayAndWrite
                   END-IF.
           EXIT.

           *> Count existing accounts
           countAccounts.
               MOVE 0 TO accountCount
               MOVE 'N' TO endOfFile
               OPEN INPUT accountFile
               PERFORM UNTIL endOfFile = "Y"
                   READ accountFile
                       AT END
                           MOVE "Y" TO endOfFile
                       NOT AT END
                           ADD 1 TO accountCount
                   END-READ
               END-PERFORM.
               CLOSE accountFile.
           EXIT.

           *> Display message and write to output file, needed for every user prompt
           *> MOVE "message" to messageVar
           *> displayAndWrite.
           displayAndWrite.
               DISPLAY messageVar
               MOVE messageVar TO userOutputRecord
               WRITE userOutputRecord
               EXIT.
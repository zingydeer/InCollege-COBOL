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
           01 newUser        PIC X.
           01 userName       PIC X(30).
           01 userPassword   PIC X(30).
           01 passwordValid  PIC X VALUE "N".

           01 accountCount PIC 9 VALUE 0.
           01 endOfFile PIC X VALUE "N".


       PROCEDURE DIVISION.
           OPEN INPUT userInputFile
           OPEN OUTPUT userOutputFile
           READ userInputFile INTO newUser.
           READ userInputFile INTO userName.
           READ userInputFile INTO userPassword.


           PERFORM countAccounts.
           OPEN EXTEND accountFile *> Truncates instead of overwriting.

           DISPLAY "Welcome to inCollege by team Wyoming!".
           DISPLAY "Are you a new user? (Y/N)".

           IF newUser = "Y" OR newUser = "y"
               PERFORM newUserRegistration
           ELSE
               PERFORM existingUserLogin
           END-IF.

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
                       DISPLAY "Please try again. Password must be 8 or 12 characters long.".
               EXIT.

           *> Existing user login process
           existingUserLogin.
               DISPLAY "Please enter your login credentials.".
               DISPLAY "Login process completed.".
               EXIT.


           *> Validate password length
           validatePassword.
                IF FUNCTION LENGTH(FUNCTION TRIM(userPassword TRAILING))  = 8 OR FUNCTION LENGTH(FUNCTION TRIM(userPassword TRAILING)) = 12
                    MOVE "Y" TO passwordValid
                ELSE
                    DISPLAY "Invalid password length: " FUNCTION LENGTH(userPassword)
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
               DISPLAY "BOY IF YOU DONT"
               DISPLAY accountCount " accounts found."

           EXIT.

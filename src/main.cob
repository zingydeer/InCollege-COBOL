      >>SOURCE FORMAT FREE
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
           01 loginInput        PIC X.
           01 userName       PIC X(30).
           01 userPassword   PIC X(30).
           01 messageVar     PIC X(100).

           *>New Password validation variables
           01 passwordValid    PIC X VALUE "N".
           01 i                PIC 99 VALUE 1.
           01 upperFound       PIC X VALUE "N".
           01 digitFound       PIC X VALUE "N".
           01 specialFound     PIC X VALUE "N".
           01 char             PIC X.
           01 passwordLength   PIC 99.

           *>Login validation variables
           01 loginSuccessful    PIC X VALUE "N".
           01 inputUsername      PIC X(30).
           01 inputPassword      PIC X(30).
           01 foundAccount       PIC X VALUE "N".
           01 currentAccount     PIC X(100).
           01 currentUsername    PIC X(30).
           01 currentPassword    PIC X(30).

           *> Pre-Login navigation variables
           01 quitProgram     PIC X VALUE "N".

           *>Post-login navigation variables
           01 menuChoice       PIC X(100).
           01 exitMenu         PIC X VALUE "N".
           01 exitSkills       PIC X VALUE "N".
           01 exitSearch       PIC X VALUE "N".
           01 exitSomeone      PIC X VALUE "N".

           *>Account counting and verification variables
           01 accountCount    PIC 9 VALUE 0.
           01 endOfFile       PIC X VALUE "N".


       PROCEDURE DIVISION.
           OPEN INPUT userInputFile
           OPEN EXTEND userOutputFile

           Move "Welcome to inCollege by Team Wyoming!" TO messageVar
           PERFORM displayAndWrite.

           MOVE "N" TO quitProgram
           PERFORM UNTIL quitProgram = "Y"
               PERFORM countAccounts

               MOVE "Login or Quit? (L/Q)" TO messageVar
               PERFORM displayAndWrite

               READ userInputFile INTO userInputRecord
                   AT END
                       MOVE "Y" TO quitProgram
                   NOT AT END
                       MOVE userInputRecord TO loginInput

                       *> Handle quit option
                       IF loginInput = "Q" OR loginInput = "q"
                           MOVE "Y" TO quitProgram
                           MOVE "Thank you for using inCollege" TO messageVar
                           PERFORM displayAndWrite
                       ELSE
                           *> Handle Delete account.txt option (secret option)
                           IF loginInput = "X" OR loginInput = "x"
                               PERFORM clearAccountsFile
                           ELSE
                               IF loginInput = "L" OR loginInput = "l"
                                   MOVE "Are you a new user? (Y/N)" TO messageVar
                                   PERFORM displayAndWrite

                                   READ userInputFile INTO userInputRecord
                                       AT END
                                           MOVE "Y" TO quitProgram
                                       NOT AT END
                                           MOVE userInputRecord TO loginInput

                                           IF loginInput = "Y" OR loginInput = "y"
                                               IF accountCount >= 5
                                                   DISPLAY "All permitted accounts have been created, please come back later."
                                                   CLOSE userInputFile
                                                   CLOSE userOutputFile
                                                   STOP RUN
                                               ELSE
                                                   PERFORM newUserRegistration
                                                   PERFORM postLoginMenu
                                               END-IF
                                           ELSE
                                               IF loginInput = "N" OR loginInput = "n"
                                                   PERFORM existingUserLogin
                                               END-IF
                                           END-IF
                                   END-READ
                               END-IF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE userInputFile.
           CLOSE userOutputFile.
           STOP RUN.

           *> *****************Subroutines to be called*****************

           *> Display message and write to output file, needed for every user prompt
           *> MOVE "message" to messageVar
           *> displayAndWrite.
           displayAndWrite.
               DISPLAY messageVar
               MOVE messageVar TO userOutputRecord
               WRITE userOutputRecord
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

           *> Validate password length
           validatePassword.
               MOVE FUNCTION LENGTH(FUNCTION TRIM(userPassword TRAILING)) TO passwordLength
               IF passwordLength >= 8 AND passwordLength <= 12
                *> Reset validation flags
                   MOVE "N" TO passwordValid
                   MOVE "N" TO upperFound
                   MOVE "N" TO digitFound
                   MOVE "N" TO specialFound

                   *> Check character requirements
                   PERFORM Varying i FROM 1 BY 1 UNTIL i > passwordLength
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
                   END-PERFORM

                   *> Character validation logic
                   IF upperFound = "Y" AND digitFound = "Y" AND specialFound = "Y"
                       MOVE "Y" TO passwordValid
                   ELSE
                       MOVE "Password must contain uppercase, digit, and special character." TO messageVar
                       PERFORM displayAndWrite
                   END-IF
               ELSE
                   MOVE "Invalid password length. Must be 8-12 characters." TO messageVar
                   PERFORM displayAndWrite
               END-IF.
           EXIT.

           validateLoginCredentials.
               MOVE "N" TO foundAccount
               MOVE "N" TO endOfFile

               *> Open account file for reading
               OPEN INPUT accountFile

               *> Read through all accounts to find match
               PERFORM UNTIL endOfFile = "Y"
                   READ accountFile INTO currentAccount
                       AT END
                           MOVE "Y" TO endOfFile
                       NOT AT END
                           *> Extract username and password from account record
                           MOVE currentAccount(1:30) TO currentUsername
                           MOVE currentAccount(31:60) TO currentPassword

                           *> Compare with input credentials
                           IF inputUsername = currentUsername AND inputPassword = currentPassword
                               MOVE "Y" TO foundAccount
                           END-IF
                   END-READ
               END-PERFORM

               IF foundAccount = "N"
                   MOVE "Account not found. Please try again." TO messageVar
                   PERFORM displayAndWrite
               END-IF

               CLOSE accountFile
               EXIT.

           *> New user registration process
           newUserRegistration.
               READ userInputFile INTO userName.
               READ userInputFile INTO userPassword.
               PERFORM validatePassword.
               IF passwordValid = "Y"
                   OPEN EXTEND accountFile
                   MOVE userName TO accountRecord(1:30)
                   MOVE userPassword TO accountRecord(31:60)
                   WRITE accountRecord
                   CLOSE accountFile
                   DISPLAY "Account Created."
               ELSE
                   MOVE "Please try again. Password must be 8 or 12 characters long." TO messageVar
                   PERFORM displayAndWrite
               END-IF
               EXIT.

           *> Existing user login process (NEEDS TO BE IMPLEMENTED)
           existingUserLogin.
               MOVE "N" TO loginSuccessful
               PERFORM UNTIL loginSuccessful = "Y"
                   *> Read login credentials from input file
                   READ userInputFile INTO inputUsername
                       AT END
                           MOVE "Y" TO loginSuccessful
                           EXIT PARAGRAPH
                       NOT AT END
                           READ userInputFile INTO inputPassword
                               AT END
                                   MOVE "Y" TO loginSuccessful
                                   EXIT PARAGRAPH
                               NOT AT END
                                   *> Validate login credentials against stored accounts
                                   PERFORM validateLoginCredentials

                                   IF foundAccount = "Y"
                                       MOVE "Y" TO loginSuccessful
                                       MOVE "You have successfully logged in" TO messageVar
                                       PERFORM displayAndWrite
                                   ELSE
                                       MOVE "Incorrect username/password, please try again" TO messageVar
                                       PERFORM displayAndWrite
                                   END-IF
                           END-READ
                   END-READ
               END-PERFORM

               PERFORM postLoginMenu
               EXIT.

           *> Post-login main menu
           postLoginMenu.
               STRING "Welcome, " DELIMITED BY SIZE
                      inputUsername DELIMITED BY SIZE
                      "!" DELIMITED BY SIZE
                   INTO messageVar
               END-STRING
               PERFORM displayAndWrite

               MOVE "N" TO exitMenu
               PERFORM UNTIL exitMenu = "Y"

               READ userInputFile INTO userInputRecord
                   AT END
                       MOVE "Y" TO exitMenu
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO menuChoice
               END-READ

                   MOVE "Search for a job" TO messageVar
                   PERFORM displayAndWrite
                   MOVE "Find someone you know" TO messageVar
                   PERFORM displayAndWrite
                   MOVE "Learn a new skill" TO messageVar
                   PERFORM displayAndWrite
                   MOVE "Log out" TO messageVar
                   PERFORM displayAndWrite


                   IF menuChoice = "Search for a job" OR menuChoice = "1"
                       PERFORM searchForJobMenu
                   ELSE
                       IF menuChoice = "Find someone you know" OR menuChoice = "2"
                           PERFORM findSomeoneMenu
                       ELSE
                           IF menuChoice = "Learn a new skill" OR menuChoice = "3"
                               PERFORM learnSkillsMenu
                            ELSE
                               IF menuChoice = "Log out" OR menuChoice = "6"
                                   MOVE "Y" TO exitMenu
                               ELSE
                                   MOVE "Invalid choice, please try again." TO messageVar
                                   PERFORM displayAndWrite
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               EXIT.

           *> Learn a new skill submenu
           learnSkillsMenu.

               MOVE "N" TO exitSkills
               PERFORM UNTIL exitSkills = "Y"
                   MOVE "Learn a New Skill:" TO messageVar
                   PERFORM displayAndWrite
                   MOVE "Skill 1" TO messageVar
                   PERFORM displayAndWrite
                   MOVE "Skill 2" TO messageVar
                   PERFORM displayAndWrite
                   MOVE "Skill 3" TO messageVar
                   PERFORM displayAndWrite
                   MOVE "Skill 4" TO messageVar
                   PERFORM displayAndWrite
                   MOVE "Skill 5" TO messageVar
                   PERFORM displayAndWrite
                   MOVE "Go Back" TO messageVar
                   PERFORM displayAndWrite
                   MOVE "Enter your choice:" TO messageVar
                   PERFORM displayAndWrite


               READ userInputFile INTO userInputRecord
                   AT END
                       MOVE "Y" TO exitSkills
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO menuChoice
               END-READ

                   IF menuChoice = "Go Back" OR menuChoice = "6"
                       MOVE "Y" TO exitSkills
                   ELSE
                       IF menuChoice = "Skill 1" OR menuChoice = "1"
                           MOVE "This skill is under construction." TO messageVar
                           PERFORM displayAndWrite
                       ELSE
                           IF menuChoice = "Skill 2" OR menuChoice = "2"
                               MOVE "This skill is under construction." TO messageVar
                               PERFORM displayAndWrite
                           ELSE
                               IF menuChoice = "Skill 3" OR menuChoice = "3"
                                   MOVE "This skill is under construction." TO messageVar
                                   PERFORM displayAndWrite
                               ELSE
                                   IF menuChoice = "Skill 4" OR menuChoice = "4"
                                       MOVE "This skill is under construction." TO messageVar
                                       PERFORM displayAndWrite
                                   ELSE
                                       IF menuChoice = "Skill 5" OR menuChoice = "5"
                                           MOVE "This skill is under construction." TO messageVar
                                           PERFORM displayAndWrite
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               EXIT.

           *> Search for job functionality (to be implemented)
           searchForJobMenu.
               MOVE "Search for a job functionality is under construction." TO messageVar
               PERFORM displayAndWrite

               MOVE "N" TO exitSearch
               PERFORM UNTIL exitSearch = "Y"

                   MOVE "Go Back" TO messageVar
                   PERFORM displayAndWrite
                   MOVE "Enter your choice:" TO messageVar
                   PERFORM displayAndWrite

               READ userInputFile INTO userInputRecord
                   AT END
                       MOVE "Y" TO exitSearch
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO menuChoice
               END-READ

               IF menuChoice = "Go Back" OR menuChoice = "6"
                   MOVE "Y" TO exitSearch
               END-IF
               END-PERFORM
               EXIT.

           *> Find someone you know functionality (to be implemented)
           findSomeoneMenu.
                MOVE "Find someone you know functionality is under construction." TO messageVar
                PERFORM displayAndWrite

                MOVE "N" TO exitSomeone
                PERFORM UNTIL exitSomeone = "Y"

                     MOVE "Go Back" TO messageVar
                     PERFORM displayAndWrite
                     MOVE "Enter your choice:" TO messageVar
                     PERFORM displayAndWrite

                   READ userInputFile INTO userInputRecord
                       AT END
                           MOVE "Y" TO exitSomeone
                           EXIT PARAGRAPH
                       NOT AT END
                           MOVE userInputRecord TO menuChoice
                   END-READ

                      IF menuChoice = "Go Back" OR menuChoice = "6"
                           MOVE "Y" TO exitSomeone
                      END-IF

                END-PERFORM
           EXIT.

           clearAccountsFile.
               OPEN OUTPUT accountFile
               CLOSE accountFile
           EXIT.

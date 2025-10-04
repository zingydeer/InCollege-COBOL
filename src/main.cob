>>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       AUTHOR. TEAM WYOMING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT userInputFile ASSIGN TO "src/files/input.txt"
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT userOutputFile ASSIGN TO "src/files/output.txt"
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT accountFile ASSIGN TO "src/files/account.txt"
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT profileFile ASSIGN TO "src/files/profile.txt"
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT tempProfileFileHandle ASSIGN TO "src/files/temp_profile.txt"
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT connectionFile ASSIGN TO "src/files/connections.txt"
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT tempConnectionFile ASSIGN TO "src/files/temp_connections.txt"
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT establishedConnectionFile ASSIGN TO "src/files/established_connections.txt"
                  ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
           FD  userInputFile.
           01  userInputRecord                PIC X(100).

           FD  userOutputFile.
           01  userOutputRecord               PIC X(100).

           FD  accountFile.
           *> username (30) + password (30) padded/trailing in X(100)
           01  accountRecord                  PIC X(100).

           FD  profileFile.
           01  profileRecord                  PIC X(2000).

           FD  tempProfileFileHandle.
           01  tempProfileFileRecord          PIC X(2000).

           FD  connectionFile.
           *> pending request: sender|recipient (30 + 1 + 30)
           01  connectionRecord               PIC X(61).

           FD  tempConnectionFile.
           01  tempConnectionRecord           PIC X(61).

           FD  establishedConnectionFile.
           *> established: userA|userB (30 + 1 + 30)
           01  establishedConnectionRecord    PIC X(61).

       WORKING-STORAGE SECTION.
       *> ---------- General I/O ----------
           01  loginInput                         PIC X.
           01  userName                           PIC X(30).
           01  userPassword                       PIC X(30).
           01  messageVar                         PIC X(150).

       *> ---------- Password validation ----------
           01  passwordValid                      PIC X VALUE "N".
           01  i                                  PIC 99 VALUE 1.
           01  upperFound                         PIC X VALUE "N".
           01  digitFound                         PIC X VALUE "N".
           01  specialFound                       PIC X VALUE "N".
           01  char                               PIC X.
           01  passwordLength                     PIC 99.

       *> ---------- Login / accounts ----------
           01  loginSuccessful                    PIC X VALUE "N".
           01  inputUsername                      PIC X(30).
           01  inputPassword                      PIC X(30).
           01  foundAccount                       PIC X VALUE "N".
           01  currentAccount                     PIC X(100).
           01  currentUsername                    PIC X(30).
           01  currentPassword                    PIC X(30).
           01  quitProgram                        PIC X VALUE "N".
           01  accountCount                       PIC 9 VALUE 0.
           01  endOfFile                          PIC X VALUE "N".

       *> ---------- Menus ----------
           01  menuChoice                         PIC X(100).
           01  exitMenu                           PIC X VALUE "N".
           01  exitSkills                         PIC X VALUE "N".
           01  exitSearch                         PIC X VALUE "N".
           01  exitSomeone                        PIC X VALUE "N".

       *> ---------- Profile data ----------
           01  profileData.
               02 profileUserName                 PIC X(30).
               02 firstName                       PIC X(30).
               02 lastName                        PIC X(30).
               02 university                      PIC X(50).
               02 major                           PIC X(30).
               02 graduationYear                  PIC 9(4).
               02 aboutMe                         PIC X(200).
               02 experienceCount                 PIC 9 VALUE 0.
               02 experience OCCURS 3 TIMES.
                  03 expTitle                     PIC X(50).
                  03 expCompany                   PIC X(50).
                  03 expDates                     PIC X(30).
                  03 expDesc                      PIC X(100).
               02 educationCount                  PIC 9 VALUE 0.
               02 education OCCURS 3 TIMES.
                  03 eduDegree                    PIC X(50).
                  03 eduUniversity                PIC X(50).
                  03 eduYears                     PIC X(20).

       *> ---------- Profile helpers ----------
           01  profileValid                       PIC X VALUE "N".
           01  yearValid                          PIC X VALUE "N".
           01  currentYear                        PIC 9(4) VALUE 2024.
           01  minYear                            PIC 9(4) VALUE 1900.
           01  maxYear                            PIC 9(4) VALUE 2100.
           01  tempYear                           PIC 9(4).
           01  tempYearX                          PIC X(4).
           01  profileChoice                      PIC X(100).
           01  profileExit                        PIC X VALUE "N".
           01  updateChoice                       PIC X(100).
           01  entryIndex                         PIC 9 VALUE 0.
           01  j                                  PIC 9 VALUE 0.
           01  tempString                         PIC X(100).
           01  tempProfileRecord                  PIC X(2000).
           01  userFound                          PIC X VALUE "N".
           01  tempProfileFile                    PIC X(50) VALUE "src/files/temp_profile.txt".

       *> ---------- “Find someone” ----------
           01  queryFirstName                     PIC X(30).
           01  queryLastName                      PIC X(30).
           01  profileFound                       PIC X VALUE "N".
           01  originalUsername                   PIC X(30).
           01  targetUsername                     PIC X(30).

       *> ---------- Pending request (sender|recipient = 61) ----------
           01  connectionData.
               05 senderUsername                  PIC X(30).
               05 sep1                            PIC X  VALUE '|'.
               05 recipientUsername               PIC X(30).
           01  pendingRequestsFound               PIC X VALUE "N".
           01  requestAlreadyExists               PIC X VALUE "N".
           01  existingConnectionRecord           PIC X(61).

       *> ---------- Established connection (userA|userB = 61) ----------
           01  establishedConnectionData.
               05 connectedUser1                  PIC X(30).
               05 sep2                            PIC X  VALUE '|'.
               05 connectedUser2                  PIC X(30).
           01  networkChoice                      PIC X(100).
           01  requestChoice                      PIC X(100).
           01  user1Username                      PIC X(30).
           01  user2Username                      PIC X(30).

       PROCEDURE DIVISION.
           OPEN INPUT userInputFile
           OPEN OUTPUT userOutputFile.

           MOVE "Welcome to inCollege by Team Wyoming!" TO messageVar
           PERFORM displayAndWrite.

           MOVE "N" TO quitProgram
           PERFORM UNTIL quitProgram = "Y"
               PERFORM countAccounts

               MOVE "Login or Quit? (L/Q)" TO messageVar
               PERFORM displayAndWrite

               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO quitProgram
                   NOT AT END
                       MOVE userInputRecord TO loginInput

                       IF loginInput = "Q" OR loginInput = "q"
                           MOVE "Y" TO quitProgram
                           MOVE "Thank you for using inCollege" TO messageVar
                           PERFORM displayAndWrite
                       ELSE
                       IF loginInput = "X" OR loginInput = "x"
                           PERFORM clearFiles
                       ELSE
                       IF loginInput = "L" OR loginInput = "l"
                           MOVE "Are you a new user? (Y/N)" TO messageVar
                           PERFORM displayAndWrite

                           READ userInputFile INTO userInputRecord
                               AT END MOVE "Y" TO quitProgram
                               NOT AT END
                                   MOVE userInputRecord TO loginInput
                                   IF loginInput = "Y" OR loginInput = "y"
                                       IF accountCount >= 5
                                           MOVE "All permitted accounts have been created, please come back later." TO messageVar
                                           PERFORM displayAndWrite
                                       ELSE
                                           PERFORM newUserRegistration
                                           IF passwordValid = "Y" AND quitProgram = "N"
                                               PERFORM postLoginMenu
                                           END-IF
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
           END-PERFORM.

           CLOSE userInputFile
           CLOSE userOutputFile.
           STOP RUN.

       *> ***************** Subroutines *****************

       displayAndWrite.
           DISPLAY FUNCTION TRIM(messageVar)
           MOVE messageVar TO userOutputRecord
           WRITE userOutputRecord
           EXIT.

       countAccounts.
           MOVE 0 TO accountCount
           MOVE 'N' TO endOfFile
           OPEN INPUT accountFile
           PERFORM UNTIL endOfFile = "Y"
               READ accountFile
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END ADD 1 TO accountCount
               END-READ
           END-PERFORM
           CLOSE accountFile
           EXIT.

       clearFiles.
           OPEN OUTPUT accountFile
           CLOSE accountFile
           OPEN OUTPUT profileFile
           CLOSE profileFile
           OPEN OUTPUT connectionFile
           CLOSE connectionFile
           OPEN OUTPUT establishedConnectionFile
           CLOSE establishedConnectionFile
           MOVE "All files cleared." TO messageVar
           PERFORM displayAndWrite
           EXIT.

       validatePassword.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(userPassword TRAILING)) TO passwordLength
           IF passwordLength >= 8 AND passwordLength <= 12
               MOVE "N" TO passwordValid upperFound digitFound specialFound
               PERFORM VARYING i FROM 1 BY 1 UNTIL i > passwordLength
                   MOVE userPassword(i:1) TO char
                   IF char >= "A" AND char <= "Z" MOVE "Y" TO upperFound END-IF
                   IF char >= "0" AND char <= "9" MOVE "Y" TO digitFound END-IF
                   IF char = "!" OR char = "@" OR char = "#" OR char = "$" OR
                      char = "%" OR char = "^" OR char = "&" OR char = "*"
                      MOVE "Y" TO specialFound
                   END-IF
               END-PERFORM
               IF upperFound = "Y" AND digitFound = "Y" AND specialFound = "Y"
                   MOVE "Y" TO passwordValid
               ELSE
                   MOVE "Password must contain uppercase, digit, and special character." TO messageVar
                   PERFORM displayAndWrite
               END-IF
           ELSE
               MOVE "Invalid password length. Must be 8-12 characters." TO messageVar
               PERFORM displayAndWrite
           END-IF
           EXIT.

       validateLoginCredentials.
           MOVE "N" TO foundAccount
           MOVE "N" TO endOfFile
           OPEN INPUT accountFile
           PERFORM UNTIL endOfFile = "Y"
               READ accountFile INTO currentAccount
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END
                       MOVE currentAccount( 1:30) TO currentUsername
                       MOVE currentAccount(31:30) TO currentPassword
                       IF FUNCTION TRIM(inputUsername) = FUNCTION TRIM(currentUsername)
                          AND FUNCTION TRIM(inputPassword) = FUNCTION TRIM(currentPassword)
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

       newUserRegistration.
           READ userInputFile INTO userName AT END MOVE "Y" TO quitProgram END-READ
           IF quitProgram = "N"
               MOVE userName TO inputUsername
               READ userInputFile INTO userPassword AT END MOVE "Y" TO quitProgram END-READ
           END-IF
           IF quitProgram = "N"
               PERFORM validatePassword
               IF passwordValid = "Y"
                   MOVE SPACES TO firstName lastName university major aboutMe
                   MOVE ZEROS  TO graduationYear
                   OPEN EXTEND accountFile
                   MOVE userName     TO accountRecord( 1:30)
                   MOVE userPassword TO accountRecord(31:30)
                   WRITE accountRecord
                   CLOSE accountFile
                   MOVE "Account Created." TO messageVar
                   PERFORM displayAndWrite
               ELSE
                   MOVE "Password does not meet requirements. Please try again." TO messageVar
                   PERFORM displayAndWrite
                   MOVE "Y" TO quitProgram
               END-IF
           END-IF
           EXIT.

       existingUserLogin.
           MOVE "N" TO loginSuccessful
           PERFORM UNTIL loginSuccessful = "Y" OR quitProgram = "Y"
               READ userInputFile INTO inputUsername AT END MOVE "Y" TO quitProgram
               NOT AT END
                   READ userInputFile INTO inputPassword AT END MOVE "Y" TO quitProgram
                   NOT AT END
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
           IF quitProgram = "N"
               PERFORM postLoginMenu
           END-IF
           EXIT.

       postLoginMenu.
           MOVE SPACES TO messageVar
           STRING "Welcome, " DELIMITED BY SIZE
                  FUNCTION TRIM(inputUsername) DELIMITED BY SIZE
                  "!" DELIMITED BY SIZE
             INTO messageVar
           END-STRING
           PERFORM displayAndWrite

           MOVE "N" TO exitMenu
           PERFORM UNTIL exitMenu = "Y" OR quitProgram = "Y"
               MOVE "1. Search for a job" TO messageVar
               PERFORM displayAndWrite
               MOVE "2. Find someone you know" TO messageVar
               PERFORM displayAndWrite
               MOVE "3. Learn a new skill" TO messageVar
               PERFORM displayAndWrite
               MOVE "4. Create/Edit My Profile" TO messageVar
               PERFORM displayAndWrite
               MOVE "5. View My Profile" TO messageVar
               PERFORM displayAndWrite
               MOVE "6. Manage Pending Connection Requests" TO messageVar
               PERFORM displayAndWrite
               MOVE "7. View My Network" TO messageVar
               PERFORM displayAndWrite
               MOVE "0. Log out" TO messageVar
               PERFORM displayAndWrite

               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO quitProgram
                   NOT AT END MOVE userInputRecord TO menuChoice
               END-READ

               IF quitProgram = "N"
                   EVALUATE FUNCTION TRIM(menuChoice)
                       WHEN "0" WHEN "Log out" MOVE "Y" TO exitMenu
                       WHEN "1" WHEN "Search for a job" PERFORM searchForJobMenu
                       WHEN "2" WHEN "Find someone you know" PERFORM findSomeoneMenu
                       WHEN "3" WHEN "Learn a new skill" PERFORM learnSkillsMenu
                       WHEN "4" WHEN "Create/Edit My Profile" PERFORM createEditProfile
                       WHEN "5" WHEN "View My Profile" PERFORM viewProfile
                       WHEN "6" WHEN "Manage Pending Connection Requests" PERFORM processConnectionRequests
                       WHEN "7" WHEN "View My Network" PERFORM viewMyNetwork
                       WHEN OTHER
                           MOVE "Invalid choice, please try again." TO messageVar
                           PERFORM displayAndWrite
                   END-EVALUATE
               END-IF
           END-PERFORM
           EXIT.

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
               MOVE "Go Back (0)" TO messageVar
               PERFORM displayAndWrite
               MOVE "Enter your choice:" TO messageVar
               PERFORM displayAndWrite

               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO exitSkills
                   NOT AT END MOVE userInputRecord TO menuChoice
               END-READ

               IF menuChoice = "Go Back" OR menuChoice = "0"
                   MOVE "Y" TO exitSkills
               ELSE
                   MOVE "This skill is under construction." TO messageVar
                   PERFORM displayAndWrite
               END-IF
           END-PERFORM
           EXIT.

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
                   AT END MOVE "Y" TO exitSearch
                   NOT AT END MOVE userInputRecord TO menuChoice
               END-READ

               IF menuChoice = "Go Back (0)" OR menuChoice = "0"
                   MOVE "Y" TO exitSearch
               END-IF
           END-PERFORM
           EXIT.

       findSomeoneMenu.
           MOVE "Please enter their first and then last name or 0 to go back." TO messageVar
           PERFORM displayAndWrite

           READ userInputFile INTO userInputRecord
               AT END EXIT PARAGRAPH
               NOT AT END MOVE userInputRecord TO menuChoice
           END-READ

           IF menuChoice = "Go Back" OR menuChoice = "0"
               EXIT PARAGRAPH
           ELSE
               MOVE menuChoice TO queryFirstName
               MOVE "Enter last name:" TO messageVar
               PERFORM displayAndWrite

               READ userInputFile INTO userInputRecord
                   AT END EXIT PARAGRAPH
                   NOT AT END MOVE userInputRecord TO queryLastName
               END-READ

               MOVE inputUsername TO originalUsername
               PERFORM findProfile
           END-IF
           EXIT.

       createEditProfile.
           MOVE "N" TO profileExit
           PERFORM UNTIL profileExit = "Y"
               MOVE "=== CREATE/EDIT PROFILE ===" TO messageVar
               PERFORM displayAndWrite
               MOVE "1. Enter Personal Information" TO messageVar
               PERFORM displayAndWrite
               MOVE "2. Update Personal Information" TO messageVar
               PERFORM displayAndWrite
               MOVE "3. Add Experience Entry" TO messageVar
               PERFORM displayAndWrite
               MOVE "4. Update Experience Entry" TO messageVar
               PERFORM displayAndWrite
               MOVE "5. Delete Experience Entry" TO messageVar
               PERFORM displayAndWrite
               MOVE "6. Add Education Entry" TO messageVar
               PERFORM displayAndWrite
               MOVE "7. Update Education Entry" TO messageVar
               PERFORM displayAndWrite
               MOVE "8. Delete Education Entry" TO messageVar
               PERFORM displayAndWrite
               MOVE "9. Save Profile" TO messageVar
               PERFORM displayAndWrite
               MOVE "0. Go Back" TO messageVar
               PERFORM displayAndWrite
               MOVE "Enter your choice:" TO messageVar
               PERFORM displayAndWrite

               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO profileExit
                   NOT AT END MOVE userInputRecord TO profileChoice
               END-READ

               IF profileChoice = "1" OR profileChoice = "Enter Personal Information"
                   PERFORM enterPersonalInfo
               ELSE IF profileChoice = "2" OR profileChoice = "Update Personal Information"
                   PERFORM updatePersonalInfo
               ELSE IF profileChoice = "3" OR profileChoice = "Add Experience Entry"
                   PERFORM addExperienceEntry
               ELSE IF profileChoice = "4" OR profileChoice = "Update Experience Entry"
                   PERFORM updateExperienceEntry
               ELSE IF profileChoice = "5" OR profileChoice = "Delete Experience Entry"
                   PERFORM deleteExperienceEntry
               ELSE IF profileChoice = "6" OR profileChoice = "Add Education Entry"
                   PERFORM addEducationEntry
               ELSE IF profileChoice = "7" OR profileChoice = "Update Education Entry"
                   PERFORM updateEducationEntry
               ELSE IF profileChoice = "8" OR profileChoice = "Delete Education Entry"
                   PERFORM deleteEducationEntry
               ELSE IF profileChoice = "9" OR profileChoice = "Save Profile"
                   PERFORM saveProfile
               ELSE IF profileChoice = "0" OR profileChoice = "Go Back"
                   MOVE "Y" TO profileExit
               ELSE
                   MOVE "Invalid choice, please try again." TO messageVar
                   PERFORM displayAndWrite
               END-IF
           END-PERFORM
           EXIT.

       enterPersonalInfo.
           MOVE "=== PERSONAL INFORMATION ===" TO messageVar
           PERFORM displayAndWrite

           MOVE "Enter First Name (Required):" TO messageVar
           PERFORM displayAndWrite
           PERFORM UNTIL firstName NOT = SPACES
               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
                   NOT AT END MOVE userInputRecord TO firstName
               END-READ
               IF firstName = SPACES
                   MOVE "First Name cannot be blank. Please enter a value." TO messageVar
                   PERFORM displayAndWrite
               END-IF
           END-PERFORM

           MOVE "Enter Last Name (Required):" TO messageVar
           PERFORM displayAndWrite
           PERFORM UNTIL lastName NOT = SPACES
               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
                   NOT AT END MOVE userInputRecord TO lastName
               END-READ
               IF lastName = SPACES
                   MOVE "Last Name cannot be blank. Please enter a value." TO messageVar
                   PERFORM displayAndWrite
               END-IF
           END-PERFORM

           MOVE "Enter University/College (Required):" TO messageVar
           PERFORM displayAndWrite
           PERFORM UNTIL university NOT = SPACES
               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
                   NOT AT END MOVE userInputRecord TO university
               END-READ
               IF university = SPACES
                   MOVE "University/College cannot be blank. Please enter a value." TO messageVar
                   PERFORM displayAndWrite
               END-IF
           END-PERFORM

           MOVE "Enter Major (Required):" TO messageVar
           PERFORM displayAndWrite
           PERFORM UNTIL major NOT = SPACES
               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
                   NOT AT END MOVE userInputRecord TO major
               END-READ
               IF major = SPACES
                   MOVE "Major cannot be blank. Please enter a value." TO messageVar
                   PERFORM displayAndWrite
               END-IF
           END-PERFORM

           MOVE "Enter Graduation Year (Required, 4 digits):" TO messageVar
           PERFORM displayAndWrite
           MOVE "N" TO yearValid
           PERFORM UNTIL yearValid = "Y" OR quitProgram = "Y"
               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO quitProgram MOVE "Y" TO yearValid
                   NOT AT END
                       MOVE userInputRecord(1:4) TO tempYearX
                       IF tempYearX IS NUMERIC
                           COMPUTE tempYear = FUNCTION NUMVAL(tempYearX)
                           PERFORM validateYear
                           IF yearValid = "Y"
                               MOVE tempYear TO graduationYear
                           ELSE
                               MOVE "Invalid year1. Please enter a valid 4-digit year." TO messageVar
                               PERFORM displayAndWrite
                           END-IF
                       ELSE
                           MOVE "N" TO yearValid
                           MOVE "Invalid year2. Please enter a valid 4-digit year." TO messageVar
                           PERFORM displayAndWrite
                       END-IF
               END-READ
           END-PERFORM

           IF quitProgram = "N"
               MOVE "Enter About Me (Optional):" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
                   NOT AT END MOVE userInputRecord TO aboutMe
               END-READ
               MOVE "Personal information entered successfully!" TO messageVar
               PERFORM displayAndWrite
           END-IF
           EXIT.

       addExperienceEntry.
           IF experienceCount >= 3
               MOVE "Maximum of 3 experience entries allowed." TO messageVar
               PERFORM displayAndWrite
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO experienceCount
           MOVE experienceCount TO j
           MOVE SPACES TO expTitle(j) expCompany(j) expDates(j) expDesc(j)

           MOVE "=== ADD EXPERIENCE ENTRY ===" TO messageVar
           PERFORM displayAndWrite

           MOVE "Enter Job Title:" TO messageVar
           PERFORM displayAndWrite
           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END MOVE userInputRecord TO expTitle(experienceCount)
           END-READ

           MOVE "Enter Company/Organization:" TO messageVar
           PERFORM displayAndWrite
           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END MOVE userInputRecord TO expCompany(experienceCount)
           END-READ

           MOVE "Enter Dates (e.g., Summer 2024):" TO messageVar
           PERFORM displayAndWrite
           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END MOVE userInputRecord TO expDates(experienceCount)
           END-READ

           MOVE "Enter Description (Optional):" TO messageVar
           PERFORM displayAndWrite
           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END MOVE userInputRecord TO expDesc(experienceCount)
           END-READ

           MOVE "Experience entry added successfully!" TO messageVar
           PERFORM displayAndWrite
           EXIT.

       addEducationEntry.
           IF educationCount >= 3
               MOVE "Maximum of 3 education entries allowed." TO messageVar
               PERFORM displayAndWrite
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO educationCount
           MOVE educationCount TO j
           MOVE SPACES TO eduDegree(j) eduUniversity(j) eduYears(j)

           MOVE "=== ADD EDUCATION ENTRY ===" TO messageVar
           PERFORM displayAndWrite

           MOVE "Enter Degree:" TO messageVar
           PERFORM displayAndWrite
           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END MOVE userInputRecord TO eduDegree(educationCount)
           END-READ

           MOVE "Enter University/College:" TO messageVar
           PERFORM displayAndWrite
           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END MOVE userInputRecord TO eduUniversity(educationCount)
           END-READ

           MOVE "Enter Years Attended (e.g., 2023-2025):" TO messageVar
           PERFORM displayAndWrite
           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END MOVE userInputRecord TO eduYears(educationCount)
           END-READ

           MOVE "Education entry added successfully!" TO messageVar
           PERFORM displayAndWrite
           EXIT.

       saveProfile.
           MOVE "N" TO userFound
           MOVE "N" TO endOfFile
           OPEN INPUT profileFile
           OPEN OUTPUT tempProfileFileHandle

           PERFORM UNTIL endOfFile = "Y"
               READ profileFile INTO profileRecord
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END
                       IF FUNCTION TRIM(profileRecord(1:30)) = FUNCTION TRIM(inputUsername)
                           MOVE "Y" TO userFound
                           MOVE SPACES TO tempProfileRecord
                           MOVE inputUsername    TO tempProfileRecord(1:30)
                           MOVE firstName        TO tempProfileRecord(31:30)
                           MOVE lastName         TO tempProfileRecord(61:30)
                           MOVE university       TO tempProfileRecord(91:50)
                           MOVE major            TO tempProfileRecord(141:30)
                           MOVE graduationYear   TO tempProfileRecord(171:4)
                           MOVE aboutMe          TO tempProfileRecord(175:200)
                           MOVE experienceCount  TO tempProfileRecord(375:1)
                           MOVE educationCount   TO tempProfileRecord(376:1)
                           MOVE expTitle(1)      TO tempProfileRecord(377:50)
                           MOVE expCompany(1)    TO tempProfileRecord(427:50)
                           MOVE expDates(1)      TO tempProfileRecord(477:30)
                           MOVE expDesc(1)       TO tempProfileRecord(507:100)
                           MOVE expTitle(2)      TO tempProfileRecord(607:50)
                           MOVE expCompany(2)    TO tempProfileRecord(657:50)
                           MOVE expDates(2)      TO tempProfileRecord(707:30)
                           MOVE expDesc(2)       TO tempProfileRecord(737:100)
                           MOVE expTitle(3)      TO tempProfileRecord(837:50)
                           MOVE expCompany(3)    TO tempProfileRecord(887:50)
                           MOVE expDates(3)      TO tempProfileRecord(937:30)
                           MOVE expDesc(3)       TO tempProfileRecord(967:100)
                           MOVE eduDegree(1)     TO tempProfileRecord(1067:50)
                           MOVE eduUniversity(1) TO tempProfileRecord(1117:50)
                           MOVE eduYears(1)      TO tempProfileRecord(1167:20)
                           MOVE eduDegree(2)     TO tempProfileRecord(1187:50)
                           MOVE eduUniversity(2) TO tempProfileRecord(1237:50)
                           MOVE eduYears(2)      TO tempProfileRecord(1287:20)
                           MOVE eduDegree(3)     TO tempProfileRecord(1307:50)
                           MOVE eduUniversity(3) TO tempProfileRecord(1357:50)
                           MOVE eduYears(3)      TO tempProfileRecord(1407:20)
                           WRITE tempProfileFileRecord FROM tempProfileRecord
                       ELSE
                           WRITE tempProfileFileRecord FROM profileRecord
                       END-IF
               END-READ
           END-PERFORM

           CLOSE profileFile
           CLOSE tempProfileFileHandle

           OPEN OUTPUT profileFile
           OPEN INPUT  tempProfileFileHandle
           MOVE "N" TO endOfFile
           PERFORM UNTIL endOfFile = "Y"
               READ tempProfileFileHandle INTO tempProfileFileRecord
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END WRITE profileRecord FROM tempProfileFileRecord
               END-READ
           END-PERFORM
           CLOSE profileFile
           CLOSE tempProfileFileHandle

           IF userFound = "N"
               OPEN EXTEND profileFile
               MOVE SPACES TO tempProfileRecord
               MOVE inputUsername    TO tempProfileRecord(1:30)
               MOVE firstName        TO tempProfileRecord(31:30)
               MOVE lastName         TO tempProfileRecord(61:30)
               MOVE university       TO tempProfileRecord(91:50)
               MOVE major            TO tempProfileRecord(141:30)
               MOVE graduationYear   TO tempProfileRecord(171:4)
               MOVE aboutMe          TO tempProfileRecord(175:200)
               MOVE experienceCount  TO tempProfileRecord(375:1)
               MOVE educationCount   TO tempProfileRecord(376:1)
               WRITE profileRecord FROM tempProfileRecord
               CLOSE profileFile
           END-IF
           EXIT.

       viewProfile.
           PERFORM loadProfile
           MOVE "=== PROFILE ===" TO messageVar
           PERFORM displayAndWrite

           MOVE SPACES TO messageVar
           STRING "Name: " DELIMITED BY SIZE
                  FUNCTION TRIM(firstName) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  FUNCTION TRIM(lastName) DELIMITED BY SIZE
             INTO messageVar
           END-STRING
           PERFORM displayAndWrite

           MOVE SPACES TO messageVar
           STRING "University: " DELIMITED BY SIZE
                  FUNCTION TRIM(university) DELIMITED BY SIZE
             INTO messageVar
           END-STRING
           PERFORM displayAndWrite

           MOVE SPACES TO messageVar
           STRING "Major: " DELIMITED BY SIZE
                  FUNCTION TRIM(major) DELIMITED BY SIZE
             INTO messageVar
           END-STRING
           PERFORM displayAndWrite

           MOVE SPACES TO messageVar
           STRING "Graduation Year: " DELIMITED BY SIZE
                  graduationYear DELIMITED BY SIZE
             INTO messageVar
           END-STRING
           PERFORM displayAndWrite

           IF aboutMe NOT = SPACES
               MOVE SPACES TO messageVar
               STRING "About Me: " DELIMITED BY SIZE
                      FUNCTION TRIM(aboutMe) DELIMITED BY SIZE
                 INTO messageVar
               END-STRING
               PERFORM displayAndWrite
           END-IF

           IF experienceCount > 0
               MOVE "Experience:" TO messageVar
               PERFORM displayAndWrite
               PERFORM VARYING j FROM 1 BY 1 UNTIL j > experienceCount
                   MOVE SPACES TO tempString
                   MOVE j TO tempString(1:1)
                   MOVE SPACES TO messageVar
                   STRING "  " DELIMITED BY SIZE
                          tempString(1:1) DELIMITED BY SIZE
                          ". " DELIMITED BY SIZE
                          FUNCTION TRIM(expTitle(j)) DELIMITED BY SIZE
                          " at " DELIMITED BY SIZE
                          FUNCTION TRIM(expCompany(j)) DELIMITED BY SIZE
                          " (" DELIMITED BY SIZE
                          FUNCTION TRIM(expDates(j)) DELIMITED BY SIZE
                          ")" DELIMITED BY SIZE
                      INTO messageVar
                   END-STRING
                   PERFORM displayAndWrite
                   IF expDesc(j) NOT = SPACES
                       MOVE SPACES TO messageVar
                       STRING "     " DELIMITED BY SIZE
                              FUNCTION TRIM(expDesc(j)) DELIMITED BY SIZE
                          INTO messageVar
                       END-STRING
                       PERFORM displayAndWrite
                   END-IF
               END-PERFORM
           END-IF

           IF educationCount > 0
               MOVE "Education:" TO messageVar
               PERFORM displayAndWrite
               PERFORM VARYING j FROM 1 BY 1 UNTIL j > educationCount
                   MOVE SPACES TO tempString
                   MOVE j TO tempString(1:1)
                   MOVE SPACES TO messageVar
                   STRING "  " DELIMITED BY SIZE
                          tempString(1:1) DELIMITED BY SIZE
                          ". " DELIMITED BY SIZE
                          FUNCTION TRIM(eduDegree(j)) DELIMITED BY SIZE
                          " from " DELIMITED BY SIZE
                          FUNCTION TRIM(eduUniversity(j)) DELIMITED BY SIZE
                          " (" DELIMITED BY SIZE
                          FUNCTION TRIM(eduYears(j)) DELIMITED BY SIZE
                          ")" DELIMITED BY SIZE
                      INTO messageVar
                   END-STRING
                   PERFORM displayAndWrite
               END-PERFORM
           END-IF

           MOVE "Press Enter to continue..." TO messageVar
           PERFORM displayAndWrite
           EXIT.

       validateYear.
           MOVE "N" TO yearValid
           IF tempYear >= minYear AND tempYear <= maxYear
               MOVE "Y" TO yearValid
           END-IF
           EXIT.

       loadProfile.
           MOVE "N" TO endOfFile
           OPEN INPUT profileFile
           PERFORM UNTIL endOfFile = "Y"
               READ profileFile INTO profileRecord
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END
                       IF FUNCTION TRIM(profileRecord(1:30)) = FUNCTION TRIM(inputUsername)
                           MOVE profileRecord(31:30)    TO firstName
                           MOVE profileRecord(61:30)    TO lastName
                           MOVE profileRecord(91:50)    TO university
                           MOVE profileRecord(141:30)   TO major
                           MOVE profileRecord(171:4)    TO graduationYear
                           MOVE profileRecord(175:200)  TO aboutMe
                           MOVE profileRecord(375:1)    TO experienceCount
                           MOVE profileRecord(376:1)    TO educationCount
                           MOVE profileRecord(377:50)   TO expTitle(1)
                           MOVE profileRecord(427:50)   TO expCompany(1)
                           MOVE profileRecord(477:30)   TO expDates(1)
                           MOVE profileRecord(507:100)  TO expDesc(1)
                           MOVE profileRecord(607:50)   TO expTitle(2)
                           MOVE profileRecord(657:50)   TO expCompany(2)
                           MOVE profileRecord(707:30)   TO expDates(2)
                           MOVE profileRecord(737:100)  TO expDesc(2)
                           MOVE profileRecord(837:50)   TO expTitle(3)
                           MOVE profileRecord(887:50)   TO expCompany(3)
                           MOVE profileRecord(937:30)   TO expDates(3)
                           MOVE profileRecord(967:100)  TO expDesc(3)
                           MOVE profileRecord(1067:50)  TO eduDegree(1)
                           MOVE profileRecord(1117:50)  TO eduUniversity(1)
                           MOVE profileRecord(1167:20)  TO eduYears(1)
                           MOVE profileRecord(1187:50)  TO eduDegree(2)
                           MOVE profileRecord(1237:50)  TO eduUniversity(2)
                           MOVE profileRecord(1257:20)  TO eduYears(2)
                           MOVE profileRecord(1277:50)  TO eduDegree(3)
                           MOVE profileRecord(1327:50)  TO eduUniversity(3)
                           MOVE profileRecord(1377:20)  TO eduYears(3)
                           MOVE "Y" TO endOfFile
                       END-IF
               END-READ
           END-PERFORM
           CLOSE profileFile
           EXIT.

       updatePersonalInfo.
           PERFORM loadProfile
           MOVE "=== UPDATE PERSONAL INFORMATION ===" TO messageVar
           PERFORM displayAndWrite

           MOVE "Current First Name: " TO messageVar
           PERFORM displayAndWrite
           MOVE firstName TO messageVar
           PERFORM displayAndWrite
           MOVE "Enter new First Name (or press Enter to keep current):" TO messageVar
           PERFORM displayAndWrite
           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END IF userInputRecord NOT = SPACES
                            MOVE userInputRecord TO firstName
                        END-IF
           END-READ

           MOVE "Current Last Name: " TO messageVar
           PERFORM displayAndWrite
           MOVE lastName TO messageVar
           PERFORM displayAndWrite
           MOVE "Enter new Last Name (or press Enter to keep current):" TO messageVar
           PERFORM displayAndWrite
           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END IF userInputRecord NOT = SPACES
                            MOVE userInputRecord TO lastName
                        END-IF
           END-READ

           MOVE "Current University: " TO messageVar
           PERFORM displayAndWrite
           MOVE university TO messageVar
           PERFORM displayAndWrite
           MOVE "Enter new University/College (or press Enter to keep current):" TO messageVar
           PERFORM displayAndWrite
           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END IF userInputRecord NOT = SPACES
                            MOVE userInputRecord TO university
                        END-IF
           END-READ

           MOVE "Current Major: " TO messageVar
           PERFORM displayAndWrite
           MOVE major TO messageVar
           PERFORM displayAndWrite
           MOVE "Enter new Major (or press Enter to keep current):" TO messageVar
           PERFORM displayAndWrite
           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END IF userInputRecord NOT = SPACES
                            MOVE userInputRecord TO major
                        END-IF
           END-READ

           MOVE "Current Graduation Year: " TO messageVar
           PERFORM displayAndWrite
           MOVE graduationYear TO messageVar
           PERFORM displayAndWrite
           MOVE "Enter new Graduation Year (or press Enter to keep current):" TO messageVar
           PERFORM displayAndWrite
           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END
                   IF userInputRecord NOT = SPACES
                       MOVE userInputRecord(1:4) TO tempYearX
                       COMPUTE tempYear = FUNCTION NUMVAL(tempYearX)
                       PERFORM validateYear
                       IF yearValid = "Y"
                           MOVE tempYear TO graduationYear
                       ELSE
                           MOVE "Invalid year. Keeping current year." TO messageVar
                           PERFORM displayAndWrite
                       END-IF
                   END-IF
           END-READ

           MOVE "Current About Me: " TO messageVar
           PERFORM displayAndWrite
           MOVE aboutMe TO messageVar
           PERFORM displayAndWrite
           MOVE "Enter new About Me (or press Enter to keep current):" TO messageVar
           PERFORM displayAndWrite
           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END IF userInputRecord NOT = SPACES
                            MOVE userInputRecord TO aboutMe
                        END-IF
           END-READ

           MOVE "Personal information updated successfully!" TO messageVar
           PERFORM displayAndWrite
           EXIT.

       updateExperienceEntry.
           PERFORM loadProfile
           IF experienceCount = 0
               MOVE "No experience entries to update." TO messageVar
               PERFORM displayAndWrite
               EXIT PARAGRAPH
           END-IF

           MOVE "=== UPDATE EXPERIENCE ENTRY ===" TO messageVar
           PERFORM displayAndWrite
           MOVE "Current Experience Entries:" TO messageVar
           PERFORM displayAndWrite

           PERFORM VARYING j FROM 1 BY 1 UNTIL j > experienceCount
               MOVE SPACES TO tempString
               MOVE j TO tempString(1:1)
               MOVE SPACES TO messageVar
               STRING "  " DELIMITED BY SIZE
                      tempString(1:1) DELIMITED BY SIZE
                      ". " DELIMITED BY SIZE
                      expTitle(j) DELIMITED BY SPACES
                      " at " DELIMITED BY SIZE
                      expCompany(j) DELIMITED BY SPACES
                 INTO messageVar
               END-STRING
               PERFORM displayAndWrite
           END-PERFORM

           MOVE "Enter the number of the entry to update (1-" TO messageVar
           PERFORM displayAndWrite
           MOVE experienceCount TO messageVar
           PERFORM displayAndWrite
           MOVE "):" TO messageVar
           PERFORM displayAndWrite

           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END MOVE userInputRecord TO updateChoice
           END-READ

           MOVE 0 TO entryIndex
           MOVE updateChoice(1:1) TO entryIndex
           IF entryIndex >= 1 AND entryIndex <= experienceCount
               MOVE "Enter new Job Title (or press Enter to keep current):" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
                   NOT AT END IF userInputRecord NOT = SPACES
                                MOVE userInputRecord TO expTitle(entryIndex)
                            END-IF
               END-READ

               MOVE "Enter new Company/Organization (or press Enter to keep current):" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
                   NOT AT END IF userInputRecord NOT = SPACES
                                MOVE userInputRecord TO expCompany(entryIndex)
                            END-IF
               END-READ

               MOVE "Enter new Dates (or press Enter to keep current):" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
                   NOT AT END IF userInputRecord NOT = SPACES
                                MOVE userInputRecord TO expDates(entryIndex)
                            END-IF
               END-READ

               MOVE "Enter new Description (or press Enter to keep current):" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
                   NOT AT END IF userInputRecord NOT = SPACES
                                MOVE userInputRecord TO expDesc(entryIndex)
                            END-IF
               END-READ

               MOVE "Experience entry updated successfully!" TO messageVar
               PERFORM displayAndWrite
           ELSE
               MOVE "Invalid entry number." TO messageVar
               PERFORM displayAndWrite
           END-IF
           EXIT.

       deleteExperienceEntry.
           PERFORM loadProfile
           IF experienceCount = 0
               MOVE "No experience entries to delete." TO messageVar
               PERFORM displayAndWrite
               EXIT PARAGRAPH
           END-IF

           MOVE "=== DELETE EXPERIENCE ENTRY ===" TO messageVar
           PERFORM displayAndWrite
           MOVE "Current Experience Entries:" TO messageVar
           PERFORM displayAndWrite

           PERFORM VARYING j FROM 1 BY 1 UNTIL j > experienceCount
               MOVE SPACES TO tempString
               MOVE j TO tempString(1:1)
               MOVE SPACES TO messageVar
               STRING "  " DELIMITED BY SIZE
                      tempString(1:1) DELIMITED BY SIZE
                      ". " DELIMITED BY SIZE
                      expTitle(j) DELIMITED BY SPACES
                      " at " DELIMITED BY SIZE
                      expCompany(j) DELIMITED BY SPACES
                 INTO messageVar
               END-STRING
               PERFORM displayAndWrite
           END-PERFORM

           MOVE "Enter the number of the entry to delete (1-" TO messageVar
           PERFORM displayAndWrite
           MOVE experienceCount TO messageVar
           PERFORM displayAndWrite
           MOVE "):" TO messageVar
           PERFORM displayAndWrite

           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END MOVE userInputRecord TO updateChoice
           END-READ

           MOVE 0 TO entryIndex
           MOVE updateChoice(1:1) TO entryIndex
           IF entryIndex >= 1 AND entryIndex <= experienceCount
               PERFORM VARYING j FROM entryIndex BY 1 UNTIL j >= experienceCount
                   MOVE expTitle(j + 1)  TO expTitle(j)
                   MOVE expCompany(j + 1) TO expCompany(j)
                   MOVE expDates(j + 1)  TO expDates(j)
                   MOVE expDesc(j + 1)   TO expDesc(j)
               END-PERFORM
               SUBTRACT 1 FROM experienceCount
               MOVE "Experience entry deleted successfully!" TO messageVar
               PERFORM displayAndWrite
           ELSE
               MOVE "Invalid entry number." TO messageVar
               PERFORM displayAndWrite
           END-IF
           EXIT.

       updateEducationEntry.
           PERFORM loadProfile
           IF educationCount = 0
               MOVE "No education entries to update." TO messageVar
               PERFORM displayAndWrite
               EXIT PARAGRAPH
           END-IF

           MOVE "=== UPDATE EDUCATION ENTRY ===" TO messageVar
           PERFORM displayAndWrite
           MOVE "Current Education Entries:" TO messageVar
           PERFORM displayAndWrite

           PERFORM VARYING j FROM 1 BY 1 UNTIL j > educationCount
               MOVE SPACES TO tempString
               MOVE j TO tempString(1:1)
               MOVE SPACES TO messageVar
               STRING "  " DELIMITED BY SIZE
                      tempString(1:1) DELIMITED BY SIZE
                      ". " DELIMITED BY SIZE
                      eduDegree(j) DELIMITED BY SPACES
                      " from " DELIMITED BY SIZE
                      eduUniversity(j) DELIMITED BY SPACES
                 INTO messageVar
               END-STRING
               PERFORM displayAndWrite
           END-PERFORM

           MOVE "Enter the number of the entry to update (1-" TO messageVar
           PERFORM displayAndWrite
           MOVE educationCount TO messageVar
           PERFORM displayAndWrite
           MOVE "):" TO messageVar
           PERFORM displayAndWrite

           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END MOVE userInputRecord TO updateChoice
           END-READ

           MOVE 0 TO entryIndex
           MOVE updateChoice(1:1) TO entryIndex
           IF entryIndex >= 1 AND entryIndex <= educationCount
               MOVE "Enter new Degree (or press Enter to keep current):" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
                   NOT AT END IF userInputRecord NOT = SPACES
                                MOVE userInputRecord TO eduDegree(entryIndex)
                            END-IF
               END-READ

               MOVE "Enter new University/College (or press Enter to keep current):" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
                   NOT AT END IF userInputRecord NOT = SPACES
                                MOVE userInputRecord TO eduUniversity(entryIndex)
                            END-IF
               END-READ

               MOVE "Enter new Years Attended (or press Enter to keep current):" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
                   NOT AT END IF userInputRecord NOT = SPACES
                                MOVE userInputRecord TO eduYears(entryIndex)
                            END-IF
               END-READ

               MOVE "Education entry updated successfully!" TO messageVar
               PERFORM displayAndWrite
           ELSE
               MOVE "Invalid entry number." TO messageVar
               PERFORM displayAndWrite
           END-IF
           EXIT.

       deleteEducationEntry.
           PERFORM loadProfile
           IF educationCount = 0
               MOVE "No education entries to delete." TO messageVar
               PERFORM displayAndWrite
               EXIT PARAGRAPH
           END-IF

           MOVE "=== DELETE EDUCATION ENTRY ===" TO messageVar
           PERFORM displayAndWrite
           MOVE "Current Education Entries:" TO messageVar
           PERFORM displayAndWrite

           PERFORM VARYING j FROM 1 BY 1 UNTIL j > educationCount
               MOVE SPACES TO tempString
               MOVE j TO tempString(1:1)
               MOVE SPACES TO messageVar
               STRING "  " DELIMITED BY SIZE
                      tempString(1:1) DELIMITED BY SIZE
                      ". " DELIMITED BY SIZE
                      eduDegree(j) DELIMITED BY SPACES
                      " from " DELIMITED BY SIZE
                      eduUniversity(j) DELIMITED BY SPACES
                 INTO messageVar
               END-STRING
               PERFORM displayAndWrite
           END-PERFORM

           MOVE "Enter the number of the entry to delete (1-" TO messageVar
           PERFORM displayAndWrite
           MOVE educationCount TO messageVar
           PERFORM displayAndWrite
           MOVE "):" TO messageVar
           PERFORM displayAndWrite

           READ userInputFile INTO userInputRecord
               AT END MOVE "Y" TO profileExit EXIT PARAGRAPH
               NOT AT END MOVE userInputRecord TO updateChoice
           END-READ

           MOVE 0 TO entryIndex
           MOVE updateChoice(1:1) TO entryIndex
           IF entryIndex >= 1 AND entryIndex <= educationCount
               PERFORM VARYING j FROM entryIndex BY 1 UNTIL j >= educationCount
                   MOVE eduDegree(j + 1)    TO eduDegree(j)
                   MOVE eduUniversity(j + 1) TO eduUniversity(j)
                   MOVE eduYears(j + 1)     TO eduYears(j)
               END-PERFORM
               SUBTRACT 1 FROM educationCount
               MOVE "Education entry deleted successfully!" TO messageVar
               PERFORM displayAndWrite
           ELSE
               MOVE "Invalid entry number." TO messageVar
               PERFORM displayAndWrite
           END-IF
           EXIT.

       findProfile.
           MOVE "N" TO profileFound
           MOVE "N" TO endOfFile
           OPEN INPUT profileFile
           PERFORM UNTIL endOfFile = "Y"
               READ profileFile INTO profileRecord
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END
                       IF (FUNCTION TRIM(profileRecord(31:30)) = FUNCTION TRIM(queryFirstName))
                        AND (FUNCTION TRIM(profileRecord(61:30)) = FUNCTION TRIM(queryLastName))
                           MOVE profileRecord(1:30) TO targetUsername
                           MOVE "Y" TO profileFound
                       END-IF
               END-READ
           END-PERFORM
           CLOSE profileFile

           IF profileFound = "N"
               MOVE "Profile not found." TO messageVar
               PERFORM displayAndWrite
               EXIT PARAGRAPH
           END-IF

           MOVE inputUsername TO originalUsername
           MOVE targetUsername TO inputUsername
           PERFORM viewProfile

           MOVE "1. Send Connection Request" TO messageVar
           PERFORM displayAndWrite
           MOVE "2. Back to Main Menu" TO messageVar
           PERFORM displayAndWrite

           READ userInputFile INTO userInputRecord
               AT END EXIT PARAGRAPH
               NOT AT END MOVE userInputRecord TO menuChoice
           END-READ

           IF menuChoice = "1"
               PERFORM sendConnectionRequest
           END-IF
           MOVE originalUsername TO inputUsername
           EXIT.

       viewPendingRequests.
           MOVE "--- Pending Connection Requests ---" TO messageVar
           PERFORM displayAndWrite

           MOVE "N" TO pendingRequestsFound
           MOVE "N" TO endOfFile
           OPEN INPUT connectionFile
           PERFORM UNTIL endOfFile = "Y"
               READ connectionFile INTO connectionRecord
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END
                       MOVE connectionRecord TO connectionData
                       IF FUNCTION TRIM(recipientUsername) = FUNCTION TRIM(inputUsername)
                           MOVE senderUsername TO messageVar
                           PERFORM displayAndWrite
                           MOVE "Y" TO pendingRequestsFound
                       END-IF
               END-READ
           END-PERFORM
           CLOSE connectionFile

           IF pendingRequestsFound = "N"
               MOVE "You have no pending connection requests at this time." TO messageVar
               PERFORM displayAndWrite
           END-IF
           EXIT.

       sendConnectionRequest.
           MOVE SPACES TO messageVar
           STRING "Sending request from "
                  FUNCTION TRIM(originalUsername)
                  " to "
                  FUNCTION TRIM(targetUsername)
                  "..."
             INTO messageVar
           END-STRING
           PERFORM displayAndWrite

           IF FUNCTION TRIM(originalUsername) = FUNCTION TRIM(targetUsername)
               MOVE "You cannot send a connection request to yourself." TO messageVar
               PERFORM displayAndWrite
               EXIT PARAGRAPH
           END-IF

           MOVE "N" TO requestAlreadyExists
           MOVE "N" TO endOfFile

           *> (1) Check duplicates in pending (either direction)
           OPEN INPUT connectionFile
           PERFORM UNTIL endOfFile = "Y"
               READ connectionFile INTO existingConnectionRecord
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END
                       MOVE existingConnectionRecord TO connectionData
                       IF (FUNCTION TRIM(senderUsername)    = FUNCTION TRIM(originalUsername)
                        AND FUNCTION TRIM(recipientUsername) = FUNCTION TRIM(targetUsername))
                        OR (FUNCTION TRIM(senderUsername)    = FUNCTION TRIM(targetUsername)
                        AND FUNCTION TRIM(recipientUsername) = FUNCTION TRIM(originalUsername))
                           MOVE "You already have a pending request with this user." TO messageVar
                           PERFORM displayAndWrite
                           MOVE "Y" TO requestAlreadyExists
                           MOVE "Y" TO endOfFile
                       END-IF
               END-READ
           END-PERFORM
           CLOSE connectionFile

           *> (2) Check duplicates in established (either direction)
           IF requestAlreadyExists = "N"
               MOVE "N" TO endOfFile
               OPEN INPUT establishedConnectionFile
               PERFORM UNTIL endOfFile = "Y"
                   READ establishedConnectionFile INTO establishedConnectionRecord
                       AT END MOVE "Y" TO endOfFile
                       NOT AT END
                           MOVE establishedConnectionRecord TO establishedConnectionData
                           IF (FUNCTION TRIM(connectedUser1) = FUNCTION TRIM(originalUsername)
                            AND FUNCTION TRIM(connectedUser2) = FUNCTION TRIM(targetUsername))
                            OR (FUNCTION TRIM(connectedUser1) = FUNCTION TRIM(targetUsername)
                            AND FUNCTION TRIM(connectedUser2) = FUNCTION TRIM(originalUsername))
                               MOVE "You are already connected with this user." TO messageVar
                               PERFORM displayAndWrite
                               MOVE "Y" TO requestAlreadyExists
                               MOVE "Y" TO endOfFile
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE establishedConnectionFile
           END-IF

           *> (3) Append pending request
           IF requestAlreadyExists = "N"
               OPEN EXTEND connectionFile
               MOVE originalUsername TO senderUsername
               MOVE '|'              TO sep1
               MOVE targetUsername   TO recipientUsername
               WRITE connectionRecord FROM connectionData
               CLOSE connectionFile

               MOVE SPACES TO messageVar
               STRING "Connection request sent to "
                      FUNCTION TRIM(targetUsername)
                      "."
                 INTO messageVar
               END-STRING
               PERFORM displayAndWrite
           END-IF
           EXIT.

       processConnectionRequests.
           MOVE "N" TO pendingRequestsFound
           MOVE "N" TO endOfFile
           MOVE SPACES TO user1Username
           MOVE SPACES TO user2Username

           *> 1) Scan for the FIRST pending request for this user
           OPEN INPUT connectionFile
           PERFORM UNTIL endOfFile = "Y"
               READ connectionFile INTO connectionRecord
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END
                       MOVE connectionRecord TO connectionData
                       IF FUNCTION TRIM(recipientUsername) = FUNCTION TRIM(inputUsername)
                           MOVE senderUsername    TO user1Username
                           MOVE recipientUsername TO user2Username
                           MOVE "Y" TO pendingRequestsFound
                           MOVE "Y" TO endOfFile
                       END-IF
               END-READ
           END-PERFORM
           CLOSE connectionFile

           IF pendingRequestsFound = "N"
               MOVE "You have no pending connection requests at this time." TO messageVar
               PERFORM displayAndWrite
               EXIT PARAGRAPH
           END-IF

           *> 2) Prompt & handle
           MOVE SPACES TO messageVar
           STRING FUNCTION TRIM(user1Username)
                  " has sent you a connection request. Accept or Reject? (A/R)"
             INTO messageVar
           END-STRING
           PERFORM displayAndWrite

           READ userInputFile INTO userInputRecord
               AT END EXIT PARAGRAPH
               NOT AT END MOVE userInputRecord TO requestChoice
           END-READ

           IF requestChoice = "A" OR requestChoice = "a"
               PERFORM acceptConnectionRequest
               MOVE SPACES TO messageVar
               STRING "You are now connected with "
                      FUNCTION TRIM(user1Username)
                      "."
                 INTO messageVar
               END-STRING
               PERFORM displayAndWrite
           ELSE IF requestChoice = "R" OR requestChoice = "r"
               PERFORM rejectConnectionRequest
               MOVE SPACES TO messageVar
               STRING "You have rejected the connection request from "
                      FUNCTION TRIM(user1Username)
                      "."
                 INTO messageVar
               END-STRING
               PERFORM displayAndWrite
           ELSE
               MOVE "Invalid choice. Skipping this request." TO messageVar
               PERFORM displayAndWrite
           END-IF
           EXIT.

       acceptConnectionRequest.
           *> Step 1: Add both directions to established
           OPEN EXTEND establishedConnectionFile

           MOVE user1Username TO connectedUser1
           MOVE '|'           TO sep2
           MOVE user2Username TO connectedUser2
           WRITE establishedConnectionRecord FROM establishedConnectionData

           MOVE user2Username TO connectedUser1
           MOVE '|'           TO sep2
           MOVE user1Username TO connectedUser2
           WRITE establishedConnectionRecord FROM establishedConnectionData

           CLOSE establishedConnectionFile

           *> Step 2: Remove this pending request from connectionFile
           MOVE "N" TO endOfFile
           OPEN INPUT  connectionFile
           OPEN OUTPUT tempConnectionFile

           PERFORM UNTIL endOfFile = "Y"
               READ connectionFile INTO connectionRecord
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END
                       MOVE connectionRecord TO connectionData
                       IF NOT ( FUNCTION TRIM(senderUsername)    = FUNCTION TRIM(user1Username)
                             AND FUNCTION TRIM(recipientUsername) = FUNCTION TRIM(user2Username) )
                           WRITE tempConnectionRecord FROM connectionRecord
                       END-IF
               END-READ
           END-PERFORM

           CLOSE connectionFile
           CLOSE tempConnectionFile

           *> Replace original with updated temp
           MOVE "N" TO endOfFile
           OPEN OUTPUT connectionFile
           OPEN INPUT  tempConnectionFile

           PERFORM UNTIL endOfFile = "Y"
               READ tempConnectionFile INTO tempConnectionRecord
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END WRITE connectionRecord FROM tempConnectionRecord
               END-READ
           END-PERFORM

           CLOSE connectionFile
           CLOSE tempConnectionFile
           EXIT.

       rejectConnectionRequest.
           MOVE "N" TO endOfFile
           OPEN INPUT  connectionFile
           OPEN OUTPUT tempConnectionFile

           PERFORM UNTIL endOfFile = "Y"
               READ connectionFile INTO connectionRecord
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END
                       MOVE connectionRecord TO connectionData
                       IF NOT ( FUNCTION TRIM(senderUsername)    = FUNCTION TRIM(user1Username)
                             AND FUNCTION TRIM(recipientUsername) = FUNCTION TRIM(user2Username) )
                           WRITE tempConnectionRecord FROM connectionRecord
                       END-IF
               END-READ
           END-PERFORM

           CLOSE connectionFile
           CLOSE tempConnectionFile

           MOVE "N" TO endOfFile
           OPEN OUTPUT connectionFile
           OPEN INPUT  tempConnectionFile
           PERFORM UNTIL endOfFile = "Y"
               READ tempConnectionFile INTO tempConnectionRecord
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END WRITE connectionRecord FROM tempConnectionRecord
               END-READ
           END-PERFORM
           CLOSE connectionFile
           CLOSE tempConnectionFile
           EXIT.

       viewMyNetwork.
           MOVE "=== MY NETWORK ===" TO messageVar
           PERFORM displayAndWrite

           MOVE "N" TO pendingRequestsFound
           MOVE "N" TO endOfFile

           OPEN INPUT establishedConnectionFile
           PERFORM UNTIL endOfFile = "Y"
               READ establishedConnectionFile INTO establishedConnectionRecord
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END
                       MOVE establishedConnectionRecord TO establishedConnectionData
                       IF FUNCTION TRIM(connectedUser1) = FUNCTION TRIM(inputUsername)
                           MOVE connectedUser2 TO targetUsername
                           PERFORM displayConnectionInfo
                           MOVE "Y" TO pendingRequestsFound
                       END-IF
               END-READ
           END-PERFORM
           CLOSE establishedConnectionFile

           IF pendingRequestsFound = "N"
               MOVE "You have no connections yet. Start connecting with other users!" TO messageVar
               PERFORM displayAndWrite
           END-IF

           MOVE "Press Enter to continue..." TO messageVar
           PERFORM displayAndWrite
           EXIT.

       displayConnectionInfo.
           MOVE "N" TO endOfFile
           OPEN INPUT profileFile
           PERFORM UNTIL endOfFile = "Y"
               READ profileFile INTO profileRecord
                   AT END MOVE "Y" TO endOfFile
                   NOT AT END
                       IF FUNCTION TRIM(profileRecord(1:30)) = FUNCTION TRIM(targetUsername)
                           MOVE SPACES TO messageVar
                           STRING "Connected with: " DELIMITED BY SIZE
                                  FUNCTION TRIM(profileRecord(31:30)) DELIMITED BY SIZE
                                  " " DELIMITED BY SIZE
                                  FUNCTION TRIM(profileRecord(61:30)) DELIMITED BY SIZE
                                  " (" DELIMITED BY SIZE
                                  FUNCTION TRIM(profileRecord(91:50)) DELIMITED BY SIZE
                                  ", " DELIMITED BY SIZE
                                  FUNCTION TRIM(profileRecord(141:30)) DELIMITED BY SIZE
                                  ")" DELIMITED BY SIZE
                              INTO messageVar
                           END-STRING
                           PERFORM displayAndWrite
                           MOVE "Y" TO endOfFile
                       END-IF
               END-READ
           END-PERFORM
           CLOSE profileFile
           EXIT.

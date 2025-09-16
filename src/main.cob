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
       SELECT profileFile ASSIGN TO "src/files/profile.txt"
                ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
           FD userInputFile.
           01 userInputRecord PIC X(100).
           FD userOutputFile.
           01 userOutputRecord PIC X(100).
           FD accountFile.
           01 accountRecord PIC X(100).
           FD profileFile.
           01 profileRecord PIC X(2000).

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

           *>Profile data structures
           01 profileData.
               02 firstName        PIC X(30).
               02 lastName         PIC X(30).
               02 university       PIC X(50).
               02 major            PIC X(30).
               02 graduationYear   PIC 9(4).
               02 aboutMe          PIC X(200).
               02 experienceCount  PIC 9 VALUE 0.
               02 experience OCCURS 3 TIMES.
                   03 expTitle     PIC X(50).
                   03 expCompany   PIC X(50).
                   03 expDates     PIC X(30).
                   03 expDesc      PIC X(100).
               02 educationCount   PIC 9 VALUE 0.
               02 education OCCURS 3 TIMES.
                   03 eduDegree    PIC X(50).
                   03 eduUniversity PIC X(50).
                   03 eduYears     PIC X(20).

           *>Profile validation variables
           01 profileValid        PIC X VALUE "N".
           01 yearValid           PIC X VALUE "N".
           01 currentYear         PIC 9(4) VALUE 2024.
           01 minYear             PIC 9(4) VALUE 1900.
           01 maxYear             PIC 9(4) VALUE 2100.
           01 tempYear            PIC 9(4).
           01 profileChoice       PIC X(100).
           01 profileExit         PIC X VALUE "N".

           *>Profile update variables
           01 updateChoice        PIC X(100).
           01 entryIndex          PIC 9 VALUE 0.
           01 j                   PIC 9 VALUE 0.
           01 tempString          PIC X(100).


       PROCEDURE DIVISION.
           OPEN INPUT userInputFile
           OPEN OUTPUT userOutputFile

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
                                                   MOVE "All permitted accounts have been created, please come back later." TO messageVar
                                                   PERFORM displayAndWrite
               ELSE
                   PERFORM newUserRegistration
                                                   IF passwordValid = "Y"
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
               MOVE userName TO inputUsername.
               READ userInputFile INTO userPassword.
               PERFORM validatePassword
               IF passwordValid = "Y"
                   OPEN EXTEND accountFile
                   MOVE userName TO accountRecord(1:30)
                   MOVE userPassword TO accountRecord(31:60)
                   WRITE accountRecord
                   CLOSE accountFile
                   DISPLAY "Account Created."
               ELSE
                   DISPLAY "Password does not meet requirements. Please try again."
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
                   MOVE "Create/Edit My Profile" TO messageVar
                   PERFORM displayAndWrite
                   MOVE "View My Profile" TO messageVar
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
                               IF menuChoice = "Create/Edit My Profile" OR menuChoice = "4"
                                   PERFORM createEditProfile
                               ELSE
                                   IF menuChoice = "View My Profile" OR menuChoice = "5"
                                       PERFORM viewProfile
                                   ELSE
                                       IF menuChoice = "Log out" OR menuChoice = "6"
                                           MOVE "Y" TO exitMenu
                                       ELSE
                                           MOVE "Invalid choice, please try again." TO messageVar
                                           PERFORM displayAndWrite
                                       END-IF
                                   END-IF
                               END-IF
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

           *> Create/Edit Profile function
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
                   MOVE "10. Go Back" TO messageVar
                   PERFORM displayAndWrite
                   MOVE "Enter your choice:" TO messageVar
                   PERFORM displayAndWrite

                   READ userInputFile INTO userInputRecord
                       AT END
                           MOVE "Y" TO profileExit
                           EXIT PARAGRAPH
                       NOT AT END
                           MOVE userInputRecord TO profileChoice
                   END-READ

                   IF profileChoice = "1" OR profileChoice = "Enter Personal Information"
                       PERFORM enterPersonalInfo
                   ELSE
                       IF profileChoice = "2" OR profileChoice = "Update Personal Information"
                           PERFORM updatePersonalInfo
                       ELSE
                           IF profileChoice = "3" OR profileChoice = "Add Experience Entry"
                               PERFORM addExperienceEntry
                           ELSE
                               IF profileChoice = "4" OR profileChoice = "Update Experience Entry"
                                   PERFORM updateExperienceEntry
                               ELSE
                                   IF profileChoice = "5" OR profileChoice = "Delete Experience Entry"
                                       PERFORM deleteExperienceEntry
                                   ELSE
                                       IF profileChoice = "6" OR profileChoice = "Add Education Entry"
                                           PERFORM addEducationEntry
                                       ELSE
                                           IF profileChoice = "7" OR profileChoice = "Update Education Entry"
                                               PERFORM updateEducationEntry
                                           ELSE
                                               IF profileChoice = "8" OR profileChoice = "Delete Education Entry"
                                                   PERFORM deleteEducationEntry
                                               ELSE
                                                   IF profileChoice = "9" OR profileChoice = "Save Profile"
                                                       PERFORM saveProfile
                                                   ELSE
                                                       IF profileChoice = "10" OR profileChoice = "Go Back"
                                                           MOVE "Y" TO profileExit
                                                       ELSE
                                                           MOVE "Invalid choice, please try again." TO messageVar
                                                           PERFORM displayAndWrite
                                                       END-IF
                                                   END-IF
                                               END-IF
                                           END-IF
                                       END-IF
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               EXIT.

           *> Enter Personal Information
           enterPersonalInfo.
               MOVE "=== PERSONAL INFORMATION ===" TO messageVar
               PERFORM displayAndWrite

               MOVE "Enter First Name (Required):" TO messageVar
               PERFORM displayAndWrite
               PERFORM UNTIL firstName NOT = SPACES
                   READ userInputFile INTO userInputRecord
                       AT END
                           MOVE "Y" TO profileExit
                           EXIT PARAGRAPH
                       NOT AT END
                           MOVE userInputRecord TO firstName
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
                       AT END
                           MOVE "Y" TO profileExit
                           EXIT PARAGRAPH
                       NOT AT END
                           MOVE userInputRecord TO lastName
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
                       AT END
                           MOVE "Y" TO profileExit
                           EXIT PARAGRAPH
                       NOT AT END
                           MOVE userInputRecord TO university
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
                       AT END
                           MOVE "Y" TO profileExit
                           EXIT PARAGRAPH
                       NOT AT END
                           MOVE userInputRecord TO major
                   END-READ
                   IF major = SPACES
                       MOVE "Major cannot be blank. Please enter a value." TO messageVar
                       PERFORM displayAndWrite
                   END-IF
               END-PERFORM

               MOVE "Enter Graduation Year (Required, 4 digits):" TO messageVar
               PERFORM displayAndWrite
               MOVE "N" TO yearValid
               PERFORM UNTIL yearValid = "Y"
                   READ userInputFile INTO userInputRecord
                       AT END
                           MOVE "Y" TO profileExit
                           EXIT PARAGRAPH
                       NOT AT END
                           MOVE userInputRecord TO tempYear
                           PERFORM validateYear
                           IF yearValid = "Y"
                               MOVE tempYear TO graduationYear
                           ELSE
                               MOVE "Invalid year. Please enter a valid 4-digit year." TO messageVar
                               PERFORM displayAndWrite
                           END-IF
                   END-READ
               END-PERFORM

               MOVE "Enter About Me (Optional):" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO aboutMe
               END-READ

               MOVE "Personal information entered successfully!" TO messageVar
               PERFORM displayAndWrite
               EXIT.

           *> Add Experience Entry
           addExperienceEntry.
               IF experienceCount >= 3
                   MOVE "Maximum of 3 experience entries allowed." TO messageVar
                   PERFORM displayAndWrite
                   EXIT
               END-IF

               ADD 1 TO experienceCount
               MOVE experienceCount TO j
               MOVE SPACES TO expTitle(j)
               MOVE SPACES TO expCompany(j)
               MOVE SPACES TO expDates(j)
               MOVE SPACES TO expDesc(j)

               MOVE "=== ADD EXPERIENCE ENTRY ===" TO messageVar
               PERFORM displayAndWrite

               MOVE "Enter Job Title:" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO expTitle(experienceCount)
               END-READ

               MOVE "Enter Company/Organization:" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO expCompany(experienceCount)
               END-READ

               MOVE "Enter Dates (e.g., Summer 2024):" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO expDates(experienceCount)
               END-READ

               MOVE "Enter Description (Optional):" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO expDesc(experienceCount)
               END-READ

               MOVE "Experience entry added successfully!" TO messageVar
               PERFORM displayAndWrite
               EXIT.

           *> Add Education Entry
           addEducationEntry.
               IF educationCount >= 3
                   MOVE "Maximum of 3 education entries allowed." TO messageVar
                   PERFORM displayAndWrite
                   EXIT
               END-IF

               ADD 1 TO educationCount
               MOVE educationCount TO j
               MOVE SPACES TO eduDegree(j)
               MOVE SPACES TO eduUniversity(j)
               MOVE SPACES TO eduYears(j)

               MOVE "=== ADD EDUCATION ENTRY ===" TO messageVar
               PERFORM displayAndWrite

               MOVE "Enter Degree:" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO eduDegree(educationCount)
               END-READ

               MOVE "Enter University/College:" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO eduUniversity(educationCount)
               END-READ

               MOVE "Enter Years Attended (e.g., 2023-2025):" TO messageVar
               PERFORM displayAndWrite
               READ userInputFile INTO userInputRecord
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO eduYears(educationCount)
               END-READ

               MOVE "Education entry added successfully!" TO messageVar
               PERFORM displayAndWrite
               EXIT.

           *> Save Profile
   *> Save Profile
saveProfile.
    OPEN EXTEND profileFile
    MOVE SPACES TO profileRecord

    MOVE inputUsername   TO profileRecord(1:30)
    MOVE firstName       TO profileRecord(31:30)
    MOVE lastName        TO profileRecord(61:30)
    MOVE university      TO profileRecord(91:50)
    MOVE major           TO profileRecord(141:30)
    MOVE graduationYear  TO profileRecord(171:4)
    MOVE aboutMe         TO profileRecord(175:200)
    MOVE experienceCount TO profileRecord(375:1)
    MOVE educationCount  TO profileRecord(376:1)

    *> ---- Blank any unused experience slots so stale data isn't re-saved
    MOVE experienceCount TO j
    ADD 1 TO j
    PERFORM VARYING j FROM j BY 1 UNTIL j > 3
        MOVE SPACES TO expTitle(j)
        MOVE SPACES TO expCompany(j)
        MOVE SPACES TO expDates(j)
        MOVE SPACES TO expDesc(j)
    END-PERFORM

    *> ---- Blank any unused education slots
    MOVE educationCount TO j
    ADD 1 TO j
    PERFORM VARYING j FROM j BY 1 UNTIL j > 3
        MOVE SPACES TO eduDegree(j)
        MOVE SPACES TO eduUniversity(j)
        MOVE SPACES TO eduYears(j)
    END-PERFORM

    *> ------- Experience (3 × 230) starting at 377
    MOVE 377 TO j
    PERFORM VARYING i FROM 1 BY 1 UNTIL i > 3
        MOVE expTitle(i)   TO profileRecord(j:50)
        ADD 50 TO j
        MOVE expCompany(i) TO profileRecord(j:50)
        ADD 50 TO j
        MOVE expDates(i)   TO profileRecord(j:30)
        ADD 30 TO j
        MOVE expDesc(i)    TO profileRecord(j:100)
        ADD 100 TO j
    END-PERFORM

    *> ------- Education (3 × 120) starts at 1067 (=377 + 690)
    MOVE 1067 TO j
    PERFORM VARYING i FROM 1 BY 1 UNTIL i > 3
        MOVE eduDegree(i)     TO profileRecord(j:50)
        ADD 50 TO j
        MOVE eduUniversity(i) TO profileRecord(j:50)
        ADD 50 TO j
        MOVE eduYears(i)      TO profileRecord(j:20)
        ADD 20 TO j
    END-PERFORM

    WRITE profileRecord
    CLOSE profileFile
    MOVE "Profile saved successfully!" TO messageVar
    PERFORM displayAndWrite
    EXIT.


           *> View Profile
           viewProfile.
               PERFORM loadProfile
               MOVE "=== MY PROFILE ===" TO messageVar
               PERFORM displayAndWrite

               STRING "Name: " DELIMITED BY SIZE
                      firstName DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      lastName DELIMITED BY SIZE
                   INTO messageVar
               END-STRING
               PERFORM displayAndWrite

               STRING "University: " DELIMITED BY SIZE
                      university DELIMITED BY SIZE
                   INTO messageVar
               END-STRING
               PERFORM displayAndWrite

               STRING "Major: " DELIMITED BY SIZE
                      major DELIMITED BY SIZE
                   INTO messageVar
               END-STRING
               PERFORM displayAndWrite

               STRING "Graduation Year: " DELIMITED BY SIZE
                      graduationYear DELIMITED BY SIZE
                   INTO messageVar
               END-STRING
               PERFORM displayAndWrite

               IF aboutMe NOT = SPACES
                   STRING "About Me: " DELIMITED BY SIZE
                          aboutMe DELIMITED BY SIZE
                       INTO messageVar
                   END-STRING
                   PERFORM displayAndWrite
               END-IF

               STRING "DEBUG: expCount=" DELIMITED BY SIZE
       experienceCount     DELIMITED BY SIZE
       " eduCount="        DELIMITED BY SIZE
       educationCount      DELIMITED BY SIZE
   INTO messageVar
END-STRING
PERFORM displayAndWrite

               IF experienceCount > 0
                   MOVE "Experience:" TO messageVar
                   PERFORM displayAndWrite
                   PERFORM VARYING j FROM 1 BY 1 UNTIL j > experienceCount
                       MOVE SPACES TO tempString
                       MOVE j TO tempString(1:1)
                       STRING "  " DELIMITED BY SIZE
                              tempString(1:1) DELIMITED BY SIZE
                              ". " DELIMITED BY SIZE
                              expTitle(j) DELIMITED BY SIZE
                              " at " DELIMITED BY SIZE
                              expCompany(j) DELIMITED BY SIZE
                              " (" DELIMITED BY SIZE
                              expDates(j) DELIMITED BY SIZE
                              ")" DELIMITED BY SIZE
                           INTO messageVar
                       END-STRING
                       PERFORM displayAndWrite
                       IF expDesc(j) NOT = SPACES
                           STRING "    " DELIMITED BY SIZE
                                  expDesc(j) DELIMITED BY SIZE
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
                       STRING "  " DELIMITED BY SIZE
                              tempString(1:1) DELIMITED BY SIZE
                              ". " DELIMITED BY SIZE
                              eduDegree(j) DELIMITED BY SIZE
                              " from " DELIMITED BY SIZE
                              eduUniversity(j) DELIMITED BY SIZE
                              " (" DELIMITED BY SIZE
                              eduYears(j) DELIMITED BY SIZE
                              ")" DELIMITED BY SIZE
                           INTO messageVar
                       END-STRING
                       PERFORM displayAndWrite
                   END-PERFORM
               END-IF

               MOVE "Press Enter to continue..." TO messageVar
               PERFORM displayAndWrite
               *> Removed the extra READ here to avoid requiring a blank line in input.txt
               EXIT.

           *> Validate Year
           validateYear.
               MOVE "N" TO yearValid
               IF tempYear >= minYear AND tempYear <= maxYear
                   MOVE "Y" TO yearValid
               END-IF
               EXIT.

           *> Load Profile
           loadProfile.
               MOVE "N" TO endOfFile
               OPEN INPUT profileFile
               PERFORM UNTIL endOfFile = "Y"
                   READ profileFile INTO profileRecord
                       AT END
                           MOVE "Y" TO endOfFile
                       NOT AT END
                           IF profileRecord(1:30) = inputUsername
                               MOVE profileRecord(31:30)   TO firstName
                               MOVE profileRecord(61:30)   TO lastName
                               MOVE profileRecord(91:50)   TO university
                               MOVE profileRecord(141:30)  TO major
                               MOVE profileRecord(171:4)   TO graduationYear
                               MOVE profileRecord(175:200) TO aboutMe
                               MOVE profileRecord(375:1)   TO experienceCount
                               MOVE profileRecord(376:1)   TO educationCount

                               *> ---- Experience block
                               MOVE 377 TO j
                               PERFORM VARYING i FROM 1 BY 1 UNTIL i > 3
                                   MOVE profileRecord(j:50)   TO expTitle(i)
                                   ADD 50 TO j
                                   MOVE profileRecord(j:50)   TO expCompany(i)
                                   ADD 50 TO j
                                   MOVE profileRecord(j:30)   TO expDates(i)
                                   ADD 30 TO j
                                   MOVE profileRecord(j:100)  TO expDesc(i)
                                   ADD 100 TO j
                               END-PERFORM

                               *> ---- Education block
                               MOVE 1067 TO j
                               PERFORM VARYING i FROM 1 BY 1 UNTIL i > 3
                                   MOVE profileRecord(j:50)   TO eduDegree(i)
                                   ADD 50 TO j
                                   MOVE profileRecord(j:50)   TO eduUniversity(i)
                                   ADD 50 TO j
                                   MOVE profileRecord(j:20)   TO eduYears(i)
                                   ADD 20 TO j
                               END-PERFORM
                           END-IF
                   END-READ
               END-PERFORM
               *> Recompute counts from loaded arrays (ignore file bytes for safety)
MOVE 0 TO experienceCount
PERFORM VARYING i FROM 1 BY 1 UNTIL i > 3
    IF expTitle(i) NOT = SPACES
        ADD 1 TO experienceCount
    END-IF
END-PERFORM

MOVE 0 TO educationCount
PERFORM VARYING i FROM 1 BY 1 UNTIL i > 3
    IF eduDegree(i) NOT = SPACES
        ADD 1 TO educationCount
    END-IF
END-PERFORM

               CLOSE profileFile
               EXIT.

           *> Update Personal Information
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
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       IF userInputRecord NOT = SPACES
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
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       IF userInputRecord NOT = SPACES
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
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       IF userInputRecord NOT = SPACES
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
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       IF userInputRecord NOT = SPACES
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
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       IF userInputRecord NOT = SPACES
                           MOVE userInputRecord TO tempYear
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
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       IF userInputRecord NOT = SPACES
                           MOVE userInputRecord TO aboutMe
                       END-IF
               END-READ

               MOVE "Personal information updated successfully!" TO messageVar
               PERFORM displayAndWrite
               EXIT.

           *> Update Experience Entry
           updateExperienceEntry.
               PERFORM loadProfile
               IF experienceCount = 0
                   MOVE "No experience entries to update." TO messageVar
                   PERFORM displayAndWrite
                   EXIT
               END-IF

               MOVE "=== UPDATE EXPERIENCE ENTRY ===" TO messageVar
               PERFORM displayAndWrite
               MOVE "Current Experience Entries:" TO messageVar
               PERFORM displayAndWrite

               PERFORM VARYING j FROM 1 BY 1 UNTIL j > experienceCount
                   MOVE SPACES TO tempString
                   MOVE j TO tempString(1:1)
                   STRING "  " DELIMITED BY SIZE
                          tempString(1:1) DELIMITED BY SIZE
                          ". " DELIMITED BY SIZE
                          expTitle(j) DELIMITED BY SIZE
                          " at " DELIMITED BY SIZE
                          expCompany(j) DELIMITED BY SIZE
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
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO updateChoice
               END-READ

               MOVE 0 TO entryIndex
               MOVE updateChoice(1:1) TO entryIndex
               IF entryIndex >= 1 AND entryIndex <= experienceCount
                   MOVE "Enter new Job Title (or press Enter to keep current):" TO messageVar
                   PERFORM displayAndWrite
                   READ userInputFile INTO userInputRecord
                       AT END
                           MOVE "Y" TO profileExit
                           EXIT PARAGRAPH
                       NOT AT END
                           IF userInputRecord NOT = SPACES
                               MOVE userInputRecord TO expTitle(entryIndex)
                           END-IF
                   END-READ

                   MOVE "Enter new Company/Organization (or press Enter to keep current):" TO messageVar
                   PERFORM displayAndWrite
                   READ userInputFile INTO userInputRecord
                       AT END
                           MOVE "Y" TO profileExit
                           EXIT PARAGRAPH
                       NOT AT END
                           IF userInputRecord NOT = SPACES
                               MOVE userInputRecord TO expCompany(entryIndex)
                           END-IF
                   END-READ

                   MOVE "Enter new Dates (or press Enter to keep current):" TO messageVar
                   PERFORM displayAndWrite
                   READ userInputFile INTO userInputRecord
                       AT END
                           MOVE "Y" TO profileExit
                           EXIT PARAGRAPH
                       NOT AT END
                           IF userInputRecord NOT = SPACES
                               MOVE userInputRecord TO expDates(entryIndex)
                           END-IF
                   END-READ

                   MOVE "Enter new Description (or press Enter to keep current):" TO messageVar
                   PERFORM displayAndWrite
                   READ userInputFile INTO userInputRecord
                       AT END
                           MOVE "Y" TO profileExit
                           EXIT PARAGRAPH
                       NOT AT END
                           IF userInputRecord NOT = SPACES
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

           *> Delete Experience Entry
           deleteExperienceEntry.
               PERFORM loadProfile
               IF experienceCount = 0
                   MOVE "No experience entries to delete." TO messageVar
                   PERFORM displayAndWrite
                   EXIT
               END-IF

               MOVE "=== DELETE EXPERIENCE ENTRY ===" TO messageVar
               PERFORM displayAndWrite
               MOVE "Current Experience Entries:" TO messageVar
               PERFORM displayAndWrite

               PERFORM VARYING j FROM 1 BY 1 UNTIL j > experienceCount
                   MOVE SPACES TO tempString
                   MOVE j TO tempString(1:1)
                   STRING "  " DELIMITED BY SIZE
                          tempString(1:1) DELIMITED BY SIZE
                          ". " DELIMITED BY SIZE
                          expTitle(j) DELIMITED BY SIZE
                          " at " DELIMITED BY SIZE
                          expCompany(j) DELIMITED BY SIZE
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
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO updateChoice
               END-READ

               MOVE 0 TO entryIndex
               MOVE updateChoice(1:1) TO entryIndex
               IF entryIndex >= 1 AND entryIndex <= experienceCount
                   PERFORM VARYING j FROM entryIndex BY 1 UNTIL j >= experienceCount
                       MOVE expTitle(j + 1) TO expTitle(j)
                       MOVE expCompany(j + 1) TO expCompany(j)
                       MOVE expDates(j + 1) TO expDates(j)
                       MOVE expDesc(j + 1) TO expDesc(j)
                   END-PERFORM
                   SUBTRACT 1 FROM experienceCount
                   MOVE "Experience entry deleted successfully!" TO messageVar
                   PERFORM displayAndWrite
               ELSE
                   MOVE "Invalid entry number." TO messageVar
                   PERFORM displayAndWrite
               END-IF
               EXIT.

           *> Update Education Entry
           updateEducationEntry.
               PERFORM loadProfile
               IF educationCount = 0
                   MOVE "No education entries to update." TO messageVar
                   PERFORM displayAndWrite
                   EXIT
               END-IF

               MOVE "=== UPDATE EDUCATION ENTRY ===" TO messageVar
               PERFORM displayAndWrite
               MOVE "Current Education Entries:" TO messageVar
               PERFORM displayAndWrite

               PERFORM VARYING j FROM 1 BY 1 UNTIL j > educationCount
                   MOVE SPACES TO tempString
                   MOVE j TO tempString(1:1)
                   STRING "  " DELIMITED BY SIZE
                          tempString(1:1) DELIMITED BY SIZE
                          ". " DELIMITED BY SIZE
                          eduDegree(j) DELIMITED BY SIZE
                          " from " DELIMITED BY SIZE
                          eduUniversity(j) DELIMITED BY SIZE
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
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO updateChoice
               END-READ

               MOVE 0 TO entryIndex
               MOVE updateChoice(1:1) TO entryIndex
               IF entryIndex >= 1 AND entryIndex <= educationCount
                   MOVE "Enter new Degree (or press Enter to keep current):" TO messageVar
                   PERFORM displayAndWrite
                   READ userInputFile INTO userInputRecord
                       AT END
                           MOVE "Y" TO profileExit
                           EXIT PARAGRAPH
                       NOT AT END
                           IF userInputRecord NOT = SPACES
                               MOVE userInputRecord TO eduDegree(entryIndex)
                           END-IF
                   END-READ

                   MOVE "Enter new University/College (or press Enter to keep current):" TO messageVar
                   PERFORM displayAndWrite
                   READ userInputFile INTO userInputRecord
                       AT END
                           MOVE "Y" TO profileExit
                           EXIT PARAGRAPH
                       NOT AT END
                           IF userInputRecord NOT = SPACES
                               MOVE userInputRecord TO eduUniversity(entryIndex)
                           END-IF
                   END-READ

                   MOVE "Enter new Years Attended (or press Enter to keep current):" TO messageVar
                   PERFORM displayAndWrite
                   READ userInputFile INTO userInputRecord
                       AT END
                           MOVE "Y" TO profileExit
                           EXIT PARAGRAPH
                       NOT AT END
                           IF userInputRecord NOT = SPACES
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

           *> Delete Education Entry
           deleteEducationEntry.
               PERFORM loadProfile
               IF educationCount = 0
                   MOVE "No education entries to delete." TO messageVar
                   PERFORM displayAndWrite
                   EXIT
               END-IF

               MOVE "=== DELETE EDUCATION ENTRY ===" TO messageVar
               PERFORM displayAndWrite
               MOVE "Current Education Entries:" TO messageVar
               PERFORM displayAndWrite

               PERFORM VARYING j FROM 1 BY 1 UNTIL j > educationCount
                   MOVE SPACES TO tempString
                   MOVE j TO tempString(1:1)
                   STRING "  " DELIMITED BY SIZE
                          tempString(1:1) DELIMITED BY SIZE
                          ". " DELIMITED BY SIZE
                          eduDegree(j) DELIMITED BY SIZE
                          " from " DELIMITED BY SIZE
                          eduUniversity(j) DELIMITED BY SIZE
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
                   AT END
                       MOVE "Y" TO profileExit
                       EXIT PARAGRAPH
                   NOT AT END
                       MOVE userInputRecord TO updateChoice
               END-READ

               MOVE 0 TO entryIndex
               MOVE updateChoice(1:1) TO entryIndex
               IF entryIndex >= 1 AND entryIndex <= educationCount
                   PERFORM VARYING j FROM entryIndex BY 1 UNTIL j >= educationCount
                       MOVE eduDegree(j + 1) TO eduDegree(j)
                       MOVE eduUniversity(j + 1) TO eduUniversity(j)
                       MOVE eduYears(j + 1) TO eduYears(j)
                   END-PERFORM
                   SUBTRACT 1 FROM educationCount
                   MOVE "Education entry deleted successfully!" TO messageVar
                   PERFORM displayAndWrite
               ELSE
                   MOVE "Invalid entry number." TO messageVar
                   PERFORM displayAndWrite
               END-IF
               EXIT.

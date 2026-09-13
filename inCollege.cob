       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "Epic1_TestCases\Epic1-CreateAccount-Positive-Test-Inputs\Epic1-CreateAccount-POS-02-ValidCapacity-MaxChar.txt"
               ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS INPUT-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO "Epic1-CreateAccount-POS-02-ValidCapacity-MaxChar.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNT-FILE ASSIGN TO "InCollege-Accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS ACCOUNT-STATUS.
           SELECT PROFILE-FILE ASSIGN TO "InCollege-Profiles.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS PROFILE-STATUS.
           SELECT TEMP-PROFILE-FILE ASSIGN TO "InCollege-Profiles.tmp"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS TEMP-PROFILE-STATUS.



       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD                    PIC X(200).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD                   PIC X(200).

       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05 FILE-USERNAME                PIC X(20).
           05 FILE-PASSWORD                PIC X(12).

       FD  PROFILE-FILE.
       01  PROFILE-RECORD.
           05 FILE-PROFILE-USERNAME       PIC X(20).
           05 FILE-FIRST-NAME             PIC X(30).
           05 FILE-LAST-NAME              PIC X(30).
           05 FILE-UNIVERSITY             PIC X(50).
           05 FILE-MAJOR                  PIC X(50).
           05 FILE-GRAD-YEAR              PIC X(4).
           05 FILE-ABOUT-ME               PIC X(200).
           05 FILE-EDUCATION-COUNT        PIC 9.
           05 FILE-EDUCATION-TABLE OCCURS 3 TIMES.
               10 FILE-EDU-DEGREE         PIC X(50).
               10 FILE-EDU-UNIVERSITY     PIC X(50).
               10 FILE-EDU-YEARS          PIC X(20).
       FD  TEMP-PROFILE-FILE.
       01  TEMP-PROFILE-RECORD.
           05 TEMP-PROFILE-USERNAME       PIC X(20).
           05 TEMP-FIRST-NAME             PIC X(30).
           05 TEMP-LAST-NAME              PIC X(30).
           05 TEMP-UNIVERSITY             PIC X(50).
           05 TEMP-MAJOR                  PIC X(50).
           05 TEMP-GRAD-YEAR              PIC X(4).
           05 TEMP-ABOUT-ME               PIC X(200).
           05 TEMP-EDUCATION-COUNT        PIC 9.
           05 TEMP-EDUCATION-TABLE OCCURS 3 TIMES.
               10 TEMP-EDU-DEGREE         PIC X(50).
               10 TEMP-EDU-UNIVERSITY     PIC X(50).
               10 TEMP-EDU-YEARS          PIC X(20).


       WORKING-STORAGE SECTION.
       01  INPUT-STATUS                    PIC XX.
       01  ACCOUNT-STATUS                  PIC XX.
       01  PROFILE-STATUS                 PIC XX.
       01  END-OF-INPUT                    PIC X VALUE "N".
           88 NO-MORE-INPUT                      VALUE "Y".

       01  INPUT-VALUE                     PIC X(200).
       01  OUTPUT-LINE                     PIC X(200).
       01  CHOICE                          PIC X(20).

       *> ACCOUNT / LOGIN VARIABLES
       01  NEW-USERNAME                    PIC X(20).
       01  NEW-PASSWORD                    PIC X(12).
       01  PASSWORD-LENGTH                 PIC 9(3).
       01  PASSWORD-POSITION               PIC 9(3).
       01  PASSWORD-CHARACTER              PIC X.
       01  HAS-UPPER                       PIC X VALUE "N".
       01  HAS-DIGIT                       PIC X VALUE "N".
       01  HAS-SPECIAL                     PIC X VALUE "N".
       01  PASSWORD-VALID                  PIC X VALUE "N".
       01  USERNAME-FOUND                  PIC X VALUE "N".
       01  LOGIN-MATCHED                   PIC X VALUE "N".

       01  ACCOUNT-COUNT                   PIC 9 VALUE ZERO.
       01  ACCOUNT-INDEX                   PIC 9 VALUE ZERO.
       01  ACCOUNT-TABLE OCCURS 5 TIMES.
           05 SAVED-USERNAME               PIC X(20).
           05 SAVED-PASSWORD               PIC X(12).

       *> CURRENT LOGGED-IN USER
       01  CURRENT-USERNAME                PIC X(20).

       *> PROFILE INFORMATION
       01  PROFILE-FIRST-NAME              PIC X(30).
       01  PROFILE-LAST-NAME               PIC X(30).
       01  PROFILE-UNIVERSITY              PIC X(50).
       01  PROFILE-MAJOR                   PIC X(50).
       01  PROFILE-GRAD-YEAR               PIC X(4).
       01  PROFILE-ABOUT-ME                PIC X(200).

       *> PROFILE STATE / VALIDATION
       01  PROFILE-EXISTS                  PIC X VALUE "N".
       01  GRAD-YEAR-VALID                 PIC X VALUE "N".

       *> EXPERIENCE
       01  EXPERIENCE-COUNT                PIC 9 VALUE ZERO.
       01  EXPERIENCE-INDEX                PIC 9 VALUE ZERO.

       01  EXPERIENCE-TABLE OCCURS 3 TIMES.
           05 EXP-TITLE                    PIC X(50).
           05 EXP-COMPANY                  PIC X(50).
           05 EXP-DATES                    PIC X(30).
           05 EXP-DESCRIPTION              PIC X(100).

       *> EDUCATION
       01  EDUCATION-COUNT                 PIC 9 VALUE ZERO.
       01  EDUCATION-INDEX                 PIC 9 VALUE ZERO.

       01  EDUCATION-TABLE OCCURS 3 TIMES.
           05 EDU-DEGREE                   PIC X(50).
           05 EDU-UNIVERSITY               PIC X(50).
           05 EDU-YEARS                    PIC X(20).

       *> MENU VARIABLES
       01  MAIN-MENU-DONE                  PIC X VALUE "N".
       01  SKILL-MENU-DONE                 PIC X VALUE "N".

       01  TEMP-PROFILE-STATUS             PIC XX.
       01  PROFILE-FOUND                   PIC X VALUE "N".

       01  PROFILE-EOF                    PIC X VALUE "N".
           88 NO-MORE-PROFILES                  VALUE "Y".

       01  TEMP-PROFILE-EOF               PIC X VALUE "N".
           88 NO-MORE-TEMP-PROFILES            VALUE "Y".




       PROCEDURE DIVISION.
       MAIN.
           OPEN OUTPUT OUTPUT-FILE
           OPEN INPUT INPUT-FILE
           IF INPUT-STATUS NOT = "00"
               MOVE "Unable to open InCollege-Input.txt." TO OUTPUT-LINE
               PERFORM EMIT-LINE
               CLOSE OUTPUT-FILE
               STOP RUN
           END-IF

           PERFORM LOAD-ACCOUNTS
           MOVE "Welcome to InCollege!" TO OUTPUT-LINE PERFORM EMIT-LINE

           PERFORM UNTIL NO-MORE-INPUT
               PERFORM ENTRY-MENU
           END-PERFORM

           CLOSE INPUT-FILE OUTPUT-FILE
           STOP RUN.

       ENTRY-MENU.
           MOVE "Log In" TO OUTPUT-LINE PERFORM EMIT-LINE
           MOVE "Create New Account" TO OUTPUT-LINE PERFORM EMIT-LINE
           MOVE "Enter your choice:" TO OUTPUT-LINE PERFORM EMIT-LINE
           PERFORM READ-INPUT

           IF NOT NO-MORE-INPUT
               MOVE FUNCTION TRIM(INPUT-VALUE) TO CHOICE
               EVALUATE CHOICE
                   WHEN "1" PERFORM LOGIN
                   WHEN "2" PERFORM CREATE-ACCOUNT
                   WHEN OTHER
                       MOVE "Invalid Choice" TO OUTPUT-LINE
                       PERFORM EMIT-LINE
               END-EVALUATE
           END-IF.

       CREATE-ACCOUNT.
           MOVE "Please enter your username:" TO OUTPUT-LINE
           PERFORM EMIT-LINE PERFORM READ-INPUT
           IF NO-MORE-INPUT EXIT PARAGRAPH END-IF
           MOVE FUNCTION TRIM(INPUT-VALUE) TO NEW-USERNAME

           MOVE "Please enter your password:" TO OUTPUT-LINE
           PERFORM EMIT-LINE PERFORM READ-INPUT
           IF NO-MORE-INPUT EXIT PARAGRAPH END-IF

           IF ACCOUNT-COUNT = 5
               MOVE "All permitted accounts have been created, " &
                   "please come back later" TO OUTPUT-LINE
               PERFORM EMIT-LINE
               EXIT PARAGRAPH
           END-IF

           IF NEW-USERNAME = SPACES
               MOVE "Username cannot be empty." TO OUTPUT-LINE
               PERFORM EMIT-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM FIND-USERNAME
           IF USERNAME-FOUND = "Y"
               MOVE "That username is already in use." TO OUTPUT-LINE
               PERFORM EMIT-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM VALIDATE-PASSWORD
           IF PASSWORD-VALID NOT = "Y"
               MOVE "Password must be 8 to 12 characters and include " &
                   "a capital letter, a digit, and a special character."
                   TO OUTPUT-LINE
               PERFORM EMIT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION TRIM(INPUT-VALUE) TO NEW-PASSWORD
           ADD 1 TO ACCOUNT-COUNT END-ADD
           MOVE NEW-USERNAME TO SAVED-USERNAME(ACCOUNT-COUNT)
           MOVE NEW-PASSWORD TO SAVED-PASSWORD(ACCOUNT-COUNT)
           PERFORM SAVE-ACCOUNT

           MOVE "Account created successfully." TO OUTPUT-LINE
           PERFORM EMIT-LINE.

       FIND-USERNAME.
           MOVE "N" TO USERNAME-FOUND
           PERFORM VARYING ACCOUNT-INDEX FROM 1 BY 1
               UNTIL ACCOUNT-INDEX > ACCOUNT-COUNT OR USERNAME-FOUND = "Y"
               IF NEW-USERNAME = SAVED-USERNAME(ACCOUNT-INDEX)
                   MOVE "Y" TO USERNAME-FOUND
               END-IF
           END-PERFORM.

       VALIDATE-PASSWORD.
           MOVE "N" TO PASSWORD-VALID HAS-UPPER HAS-DIGIT HAS-SPECIAL
           MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-VALUE))
               TO PASSWORD-LENGTH
           IF PASSWORD-LENGTH < 8 OR PASSWORD-LENGTH > 12
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING PASSWORD-POSITION FROM 1 BY 1
               UNTIL PASSWORD-POSITION > PASSWORD-LENGTH
               MOVE INPUT-VALUE(PASSWORD-POSITION:1) TO PASSWORD-CHARACTER
               EVALUATE TRUE
                   WHEN PASSWORD-CHARACTER >= "A"
                       AND PASSWORD-CHARACTER <= "Z"
                       MOVE "Y" TO HAS-UPPER
                   WHEN PASSWORD-CHARACTER >= "0"
                       AND PASSWORD-CHARACTER <= "9"
                       MOVE "Y" TO HAS-DIGIT
                   WHEN PASSWORD-CHARACTER NOT = SPACE
                       AND NOT (PASSWORD-CHARACTER >= "a"
                       AND PASSWORD-CHARACTER <= "z")
                       MOVE "Y" TO HAS-SPECIAL
               END-EVALUATE
           END-PERFORM

           IF HAS-UPPER = "Y" AND HAS-DIGIT = "Y" AND HAS-SPECIAL = "Y"
               MOVE "Y" TO PASSWORD-VALID
           END-IF.

       LOGIN.
           MOVE "N" TO LOGIN-MATCHED
           PERFORM UNTIL LOGIN-MATCHED = "Y" OR NO-MORE-INPUT
               MOVE "Please enter your username:" TO OUTPUT-LINE
               PERFORM EMIT-LINE PERFORM READ-INPUT
               IF NO-MORE-INPUT EXIT PERFORM END-IF
               MOVE FUNCTION TRIM(INPUT-VALUE) TO NEW-USERNAME

               MOVE "Please enter your password:" TO OUTPUT-LINE
               PERFORM EMIT-LINE PERFORM READ-INPUT
               IF NO-MORE-INPUT EXIT PERFORM END-IF
               MOVE FUNCTION TRIM(INPUT-VALUE) TO NEW-PASSWORD

               PERFORM CHECK-LOGIN
               IF LOGIN-MATCHED = "Y"
                   PERFORM VALID-LOGIN-HANDOFF
               ELSE
                   MOVE "Incorrect username/password, please try again"
                       TO OUTPUT-LINE
                   PERFORM EMIT-LINE
               END-IF
           END-PERFORM.

       CHECK-LOGIN.
           PERFORM VARYING ACCOUNT-INDEX FROM 1 BY 1
               UNTIL ACCOUNT-INDEX > ACCOUNT-COUNT OR LOGIN-MATCHED = "Y"
               IF NEW-USERNAME = SAVED-USERNAME(ACCOUNT-INDEX)
                   AND NEW-PASSWORD = SAVED-PASSWORD(ACCOUNT-INDEX)
                   MOVE "Y" TO LOGIN-MATCHED
                   MOVE NEW-USERNAME TO CURRENT-USERNAME
               END-IF
           END-PERFORM.

       VALID-LOGIN-HANDOFF.
            MOVE "You have successfully logged in" TO OUTPUT-LINE
            PERFORM EMIT-LINE

            PERFORM LOAD-PROFILE

            MOVE "N" TO MAIN-MENU-DONE
            PERFORM UNTIL MAIN-MENU-DONE = "Y"
                PERFORM MAIN-MENU
            END-PERFORM.

       LOAD-ACCOUNTS.
           OPEN INPUT ACCOUNT-FILE
           IF ACCOUNT-STATUS = "35"
               OPEN OUTPUT ACCOUNT-FILE
               CLOSE ACCOUNT-FILE
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL ACCOUNT-STATUS NOT = "00"
               READ ACCOUNT-FILE
                   AT END CONTINUE
                   NOT AT END
                       IF ACCOUNT-COUNT < 5
                           ADD 1 TO ACCOUNT-COUNT END-ADD
                           MOVE FILE-USERNAME TO SAVED-USERNAME(ACCOUNT-COUNT)
                           MOVE FILE-PASSWORD TO SAVED-PASSWORD(ACCOUNT-COUNT)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACCOUNT-FILE.

       SAVE-ACCOUNT.
           OPEN OUTPUT ACCOUNT-FILE
           PERFORM VARYING ACCOUNT-INDEX FROM 1 BY 1
               UNTIL ACCOUNT-INDEX > ACCOUNT-COUNT
               MOVE SAVED-USERNAME(ACCOUNT-INDEX) TO FILE-USERNAME
               MOVE SAVED-PASSWORD(ACCOUNT-INDEX) TO FILE-PASSWORD
               WRITE ACCOUNT-RECORD END-WRITE
           END-PERFORM
           CLOSE ACCOUNT-FILE.

       READ-INPUT.
           MOVE SPACES TO INPUT-RECORD INPUT-VALUE
           READ INPUT-FILE
               AT END MOVE "Y" TO END-OF-INPUT
               NOT AT END
                   MOVE INPUT-RECORD TO INPUT-VALUE OUTPUT-LINE
                   PERFORM EMIT-LINE
           END-READ.



        
       MAIN-MENU.
            IF NO-MORE-INPUT
                MOVE "Y" TO MAIN-MENU-DONE
                EXIT PARAGRAPH
            END-IF



                            *>POST LOGIN NAVIGATION OPTIONS
            MOVE "1. Search for a job" TO OUTPUT-LINE
            PERFORM EMIT-LINE
            MOVE "2. Find someone you know" TO OUTPUT-LINE
            PERFORM EMIT-LINE
            MOVE "3. Learn a new skill" TO OUTPUT-LINE
            PERFORM EMIT-LINE
            MOVE "4. Edit My Profile" TO OUTPUT-LINE
            PERFORM EMIT-LINE
            MOVE "5. View My Profile" TO OUTPUT-LINE
            PERFORM EMIT-LINE
            MOVE "Logout" TO OUTPUT-LINE
            PERFORM EMIT-LINE
            MOVE "Enter your choice:" TO OUTPUT-LINE
            PERFORM EMIT-LINE

            PERFORM READ-INPUT

            IF NO-MORE-INPUT
                MOVE "Y" TO MAIN-MENU-DONE
                EXIT PARAGRAPH
            END-IF

            MOVE FUNCTION TRIM(INPUT-VALUE) TO CHOICE


            EVALUATE CHOICE
                WHEN "1"
                    PERFORM JOB-SEARCH
                WHEN "2"
                    PERFORM FIND-SOMEONE
                WHEN "3"
                    PERFORM SKILL-MENU
                WHEN "4"
                    PERFORM EDIT-PROFILE
                WHEN "5"
                    PERFORM VIEW-PROFILE
                WHEN "Logout"
                    MOVE "Y" TO MAIN-MENU-DONE
                    MOVE "Y" TO END-OF-INPUT
                WHEN OTHER
                    MOVE "Invalid Choice" TO OUTPUT-LINE
                    PERFORM EMIT-LINE
            END-EVALUATE.

        JOB-SEARCH.
            MOVE "Job search/internship is under construction." TO OUTPUT-LINE
            PERFORM EMIT-LINE.
        
        FIND-SOMEONE.
            MOVE "Find someone you know is under construction." TO OUTPUT-LINE
            PERFORM EMIT-LINE.
        
        UNDER-CONSTRUCTION.
            MOVE "This skill is under construction." TO OUTPUT-LINE
            PERFORM EMIT-LINE.


        SKILL-MENU.
            MOVE "N" TO SKILL-MENU-DONE

            PERFORM UNTIL SKILL-MENU-DONE = "Y"

                
                MOVE "Learn a New Skill:" TO OUTPUT-LINE
                PERFORM EMIT-LINE
                MOVE "Skill 1" TO OUTPUT-LINE
                PERFORM EMIT-LINE
                MOVE "Skill 2" TO OUTPUT-LINE
                PERFORM EMIT-LINE
                MOVE "Skill 3" TO OUTPUT-LINE
                PERFORM EMIT-LINE
                MOVE "Skill 4" TO OUTPUT-LINE
                PERFORM EMIT-LINE
                MOVE "Skill 5" TO OUTPUT-LINE
                PERFORM EMIT-LINE
                MOVE "Go Back" TO OUTPUT-LINE
                PERFORM EMIT-LINE
                MOVE "Enter your choice:" TO OUTPUT-LINE
                PERFORM EMIT-LINE

                PERFORM READ-INPUT

                IF NO-MORE-INPUT
                    MOVE "Y" TO SKILL-MENU-DONE
                ELSE
                    MOVE FUNCTION TRIM(INPUT-VALUE) TO CHOICE
                

                    EVALUATE CHOICE
                        WHEN "Skill 1"
                            PERFORM UNDER-CONSTRUCTION
                        WHEN "Skill 2"
                            PERFORM UNDER-CONSTRUCTION
                        WHEN "Skill 3"
                            PERFORM UNDER-CONSTRUCTION
                        WHEN "Skill 4"
                            PERFORM UNDER-CONSTRUCTION
                        WHEN "Skill 5"
                            PERFORM UNDER-CONSTRUCTION
                        WHEN "Go Back"
                            MOVE "Y" TO SKILL-MENU-DONE
                        WHEN OTHER
                            MOVE "Invalid Choice" TO OUTPUT-LINE
                            PERFORM EMIT-LINE
                    END-EVALUATE
                END-IF
            END-PERFORM.

       LOAD-PROFILE.
           MOVE SPACES TO PROFILE-FIRST-NAME
                          PROFILE-LAST-NAME
                          PROFILE-UNIVERSITY
                          PROFILE-MAJOR
                          PROFILE-GRAD-YEAR
                          PROFILE-ABOUT-ME

           MOVE ZERO TO EDUCATION-COUNT

           PERFORM VARYING EDUCATION-INDEX FROM 1 BY 1
               UNTIL EDUCATION-INDEX > 3
               MOVE SPACES TO EDU-DEGREE(EDUCATION-INDEX)
                              EDU-UNIVERSITY(EDUCATION-INDEX)
                              EDU-YEARS(EDUCATION-INDEX)
           END-PERFORM

           MOVE "N" TO PROFILE-EXISTS

           OPEN INPUT PROFILE-FILE

           IF PROFILE-STATUS = "35"
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL PROFILE-STATUS NOT = "00"
               READ PROFILE-FILE
                   AT END CONTINUE
                   NOT AT END
                       IF FILE-PROFILE-USERNAME = CURRENT-USERNAME
                           MOVE FILE-FIRST-NAME
                               TO PROFILE-FIRST-NAME
                           MOVE FILE-LAST-NAME
                               TO PROFILE-LAST-NAME
                           MOVE FILE-UNIVERSITY
                               TO PROFILE-UNIVERSITY
                           MOVE FILE-MAJOR
                               TO PROFILE-MAJOR
                           MOVE FILE-GRAD-YEAR
                               TO PROFILE-GRAD-YEAR
                           MOVE FILE-ABOUT-ME
                               TO PROFILE-ABOUT-ME

                           MOVE FILE-EDUCATION-COUNT
                               TO EDUCATION-COUNT

                           PERFORM VARYING EDUCATION-INDEX
                               FROM 1 BY 1
                               UNTIL EDUCATION-INDEX > EDUCATION-COUNT

                               MOVE FILE-EDU-DEGREE(EDUCATION-INDEX)
                                   TO EDU-DEGREE(EDUCATION-INDEX)

                               MOVE FILE-EDU-UNIVERSITY(EDUCATION-INDEX)
                                   TO EDU-UNIVERSITY(EDUCATION-INDEX)

                               MOVE FILE-EDU-YEARS(EDUCATION-INDEX)
                                   TO EDU-YEARS(EDUCATION-INDEX)

                           END-PERFORM

                           MOVE "Y" TO PROFILE-EXISTS
                       END-IF
               END-READ
           END-PERFORM

           CLOSE PROFILE-FILE.


       EDIT-PROFILE.
           MOVE "Edit My Profile" TO OUTPUT-LINE
           PERFORM EMIT-LINE

           MOVE "Enter your first name:" TO OUTPUT-LINE
           PERFORM EMIT-LINE
           PERFORM READ-INPUT
           IF NO-MORE-INPUT EXIT PARAGRAPH END-IF
           MOVE FUNCTION TRIM(INPUT-VALUE) TO PROFILE-FIRST-NAME

           MOVE "Enter your last name:" TO OUTPUT-LINE
           PERFORM EMIT-LINE
           PERFORM READ-INPUT
           IF NO-MORE-INPUT EXIT PARAGRAPH END-IF
           MOVE FUNCTION TRIM(INPUT-VALUE) TO PROFILE-LAST-NAME

           MOVE "Enter your university:" TO OUTPUT-LINE
           PERFORM EMIT-LINE
           PERFORM READ-INPUT
           IF NO-MORE-INPUT EXIT PARAGRAPH END-IF
           MOVE FUNCTION TRIM(INPUT-VALUE) TO PROFILE-UNIVERSITY

           MOVE "Enter your major:" TO OUTPUT-LINE
           PERFORM EMIT-LINE
           PERFORM READ-INPUT
           IF NO-MORE-INPUT EXIT PARAGRAPH END-IF
           MOVE FUNCTION TRIM(INPUT-VALUE) TO PROFILE-MAJOR

           MOVE "Enter your graduation year:" TO OUTPUT-LINE
           PERFORM EMIT-LINE
           PERFORM READ-INPUT
           IF NO-MORE-INPUT EXIT PARAGRAPH END-IF
           MOVE FUNCTION TRIM(INPUT-VALUE) TO PROFILE-GRAD-YEAR

           PERFORM VALIDATE-GRAD-YEAR
           IF GRAD-YEAR-VALID NOT = "Y"
               MOVE "Invalid graduation year." TO OUTPUT-LINE
               PERFORM EMIT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE "Tell us about yourself:" TO OUTPUT-LINE
           PERFORM EMIT-LINE
           PERFORM READ-INPUT
           IF NO-MORE-INPUT EXIT PARAGRAPH END-IF
           MOVE FUNCTION TRIM(INPUT-VALUE) TO PROFILE-ABOUT-ME
           
           PERFORM ADD-EDUCATION

           PERFORM SAVE-PROFILE

           MOVE "Profile updated successfully." TO OUTPUT-LINE
           PERFORM EMIT-LINE.

       ADD-EDUCATION.
           MOVE ZERO TO EDUCATION-COUNT

           PERFORM UNTIL EDUCATION-COUNT = 3

               MOVE "Enter your degree:" TO OUTPUT-LINE
               PERFORM EMIT-LINE
               PERFORM READ-INPUT
               IF NO-MORE-INPUT
                   EXIT PERFORM
               END-IF

               ADD 1 TO EDUCATION-COUNT
               MOVE FUNCTION TRIM(INPUT-VALUE)
                   TO EDU-DEGREE(EDUCATION-COUNT)

               MOVE "Enter your university:" TO OUTPUT-LINE
               PERFORM EMIT-LINE
               PERFORM READ-INPUT
               IF NO-MORE-INPUT
                   EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(INPUT-VALUE)
                   TO EDU-UNIVERSITY(EDUCATION-COUNT)

               MOVE "Enter the years attended:" TO OUTPUT-LINE
               PERFORM EMIT-LINE
               PERFORM READ-INPUT
               IF NO-MORE-INPUT
                   EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(INPUT-VALUE)
                   TO EDU-YEARS(EDUCATION-COUNT)

               MOVE "Education entry added." TO OUTPUT-LINE
               PERFORM EMIT-LINE

               IF EDUCATION-COUNT < 3
                   MOVE "Would you like to add another education entry? (Y/N)"
                       TO OUTPUT-LINE
                   PERFORM EMIT-LINE
                   PERFORM READ-INPUT
                   IF NO-MORE-INPUT
                       EXIT PERFORM
                   END-IF

                   MOVE FUNCTION TRIM(INPUT-VALUE) TO CHOICE

                   IF CHOICE NOT = "Y" AND CHOICE NOT = "y"
                       EXIT PERFORM
                   END-IF
               END-IF

           END-PERFORM.


       VIEW-PROFILE.
           MOVE "My Profile" TO OUTPUT-LINE
           PERFORM EMIT-LINE

           MOVE "First Name: " TO OUTPUT-LINE
           MOVE PROFILE-FIRST-NAME TO OUTPUT-LINE(13:30)
           PERFORM EMIT-LINE

           MOVE "Last Name: " TO OUTPUT-LINE
           MOVE PROFILE-LAST-NAME TO OUTPUT-LINE(12:30)
           PERFORM EMIT-LINE

           MOVE "University: " TO OUTPUT-LINE
           MOVE PROFILE-UNIVERSITY TO OUTPUT-LINE(13:50)
           PERFORM EMIT-LINE

           MOVE "Major: " TO OUTPUT-LINE
           MOVE PROFILE-MAJOR TO OUTPUT-LINE(8:50)
           PERFORM EMIT-LINE

           MOVE "Graduation Year: " TO OUTPUT-LINE
           MOVE PROFILE-GRAD-YEAR TO OUTPUT-LINE(18:4)
           PERFORM EMIT-LINE

           MOVE "About Me: " TO OUTPUT-LINE
           MOVE PROFILE-ABOUT-ME TO OUTPUT-LINE(11:200)
           PERFORM EMIT-LINE

           IF EDUCATION-COUNT = 0
               MOVE "No education entries." TO OUTPUT-LINE
               PERFORM EMIT-LINE
           ELSE
               PERFORM VARYING EDUCATION-INDEX FROM 1 BY 1
                   UNTIL EDUCATION-INDEX > EDUCATION-COUNT

                   MOVE "Degree: " TO OUTPUT-LINE
                   MOVE EDU-DEGREE(EDUCATION-INDEX)
                       TO OUTPUT-LINE(9:50)
                   PERFORM EMIT-LINE

                   MOVE "University: " TO OUTPUT-LINE
                   MOVE EDU-UNIVERSITY(EDUCATION-INDEX)
                       TO OUTPUT-LINE(13:50)
                   PERFORM EMIT-LINE

                   MOVE "Years: " TO OUTPUT-LINE
                   MOVE EDU-YEARS(EDUCATION-INDEX)
                       TO OUTPUT-LINE(8:20)
                   PERFORM EMIT-LINE

               END-PERFORM
           END-IF.

       WRITE-CURRENT-PROFILE.
           MOVE CURRENT-USERNAME
               TO FILE-PROFILE-USERNAME

           MOVE PROFILE-FIRST-NAME
               TO FILE-FIRST-NAME

           MOVE PROFILE-LAST-NAME
               TO FILE-LAST-NAME

           MOVE PROFILE-UNIVERSITY
               TO FILE-UNIVERSITY

           MOVE PROFILE-MAJOR
               TO FILE-MAJOR

           MOVE PROFILE-GRAD-YEAR
               TO FILE-GRAD-YEAR

           MOVE PROFILE-ABOUT-ME
               TO FILE-ABOUT-ME

           MOVE EDUCATION-COUNT
               TO FILE-EDUCATION-COUNT

           PERFORM VARYING EDUCATION-INDEX FROM 1 BY 1
               UNTIL EDUCATION-INDEX > 3

               MOVE EDU-DEGREE(EDUCATION-INDEX)
                   TO FILE-EDU-DEGREE(EDUCATION-INDEX)

               MOVE EDU-UNIVERSITY(EDUCATION-INDEX)
                   TO FILE-EDU-UNIVERSITY(EDUCATION-INDEX)

               MOVE EDU-YEARS(EDUCATION-INDEX)
                   TO FILE-EDU-YEARS(EDUCATION-INDEX)

           END-PERFORM

           WRITE PROFILE-RECORD.
       
       WRITE-CURRENT-PROFILE-TO-TEMP.
           MOVE CURRENT-USERNAME
               TO TEMP-PROFILE-USERNAME

           MOVE PROFILE-FIRST-NAME
               TO TEMP-FIRST-NAME

           MOVE PROFILE-LAST-NAME
               TO TEMP-LAST-NAME

           MOVE PROFILE-UNIVERSITY
               TO TEMP-UNIVERSITY

           MOVE PROFILE-MAJOR
               TO TEMP-MAJOR

           MOVE PROFILE-GRAD-YEAR
               TO TEMP-GRAD-YEAR

           MOVE PROFILE-ABOUT-ME
               TO TEMP-ABOUT-ME

           MOVE EDUCATION-COUNT
               TO TEMP-EDUCATION-COUNT

           PERFORM VARYING EDUCATION-INDEX FROM 1 BY 1
               UNTIL EDUCATION-INDEX > 3

               MOVE EDU-DEGREE(EDUCATION-INDEX)
                   TO TEMP-EDU-DEGREE(EDUCATION-INDEX)

               MOVE EDU-UNIVERSITY(EDUCATION-INDEX)
                   TO TEMP-EDU-UNIVERSITY(EDUCATION-INDEX)

               MOVE EDU-YEARS(EDUCATION-INDEX)
                   TO TEMP-EDU-YEARS(EDUCATION-INDEX)

           END-PERFORM

           WRITE TEMP-PROFILE-RECORD.

       WRITE-EXISTING-PROFILE-TO-TEMP.
           MOVE FILE-PROFILE-USERNAME
               TO TEMP-PROFILE-USERNAME

           MOVE FILE-FIRST-NAME
               TO TEMP-FIRST-NAME

           MOVE FILE-LAST-NAME
               TO TEMP-LAST-NAME

           MOVE FILE-UNIVERSITY
               TO TEMP-UNIVERSITY

           MOVE FILE-MAJOR
               TO TEMP-MAJOR

           MOVE FILE-GRAD-YEAR
               TO TEMP-GRAD-YEAR

           MOVE FILE-ABOUT-ME
               TO TEMP-ABOUT-ME

           MOVE FILE-EDUCATION-COUNT
               TO TEMP-EDUCATION-COUNT

           PERFORM VARYING EDUCATION-INDEX FROM 1 BY 1
               UNTIL EDUCATION-INDEX > 3

               MOVE FILE-EDU-DEGREE(EDUCATION-INDEX)
                   TO TEMP-EDU-DEGREE(EDUCATION-INDEX)

               MOVE FILE-EDU-UNIVERSITY(EDUCATION-INDEX)
                   TO TEMP-EDU-UNIVERSITY(EDUCATION-INDEX)

               MOVE FILE-EDU-YEARS(EDUCATION-INDEX)
                   TO TEMP-EDU-YEARS(EDUCATION-INDEX)

           END-PERFORM

           WRITE TEMP-PROFILE-RECORD.



       SAVE-PROFILE.
           MOVE "N" TO PROFILE-FOUND
           MOVE "N" TO PROFILE-EOF
           MOVE "N" TO TEMP-PROFILE-EOF

           *> Try to open the existing profile file.
           OPEN INPUT PROFILE-FILE

           *> Profile file does not exist yet.
           IF PROFILE-STATUS = "35"
               CLOSE PROFILE-FILE

               OPEN OUTPUT PROFILE-FILE

               IF PROFILE-STATUS NOT = "00"
                   MOVE "Unable to create profile file."
                       TO OUTPUT-LINE
                   PERFORM EMIT-LINE
                   EXIT PARAGRAPH
               END-IF

               PERFORM WRITE-CURRENT-PROFILE

               CLOSE PROFILE-FILE

               MOVE "Y" TO PROFILE-EXISTS
               EXIT PARAGRAPH
           END-IF

           *> Some other error occurred opening the profile file.
           IF PROFILE-STATUS NOT = "00"
               MOVE "Unable to open profile file."
                   TO OUTPUT-LINE
               PERFORM EMIT-LINE
               EXIT PARAGRAPH
           END-IF

           *> Create a temporary file.
           OPEN OUTPUT TEMP-PROFILE-FILE

           IF TEMP-PROFILE-STATUS NOT = "00"
               MOVE "Unable to create temporary profile file."
                   TO OUTPUT-LINE
               PERFORM EMIT-LINE
               CLOSE PROFILE-FILE
               EXIT PARAGRAPH
           END-IF

           *> Copy all existing profiles to the temporary file.
           *> Replace the current user's profile if it exists.
           PERFORM UNTIL NO-MORE-PROFILES

               READ PROFILE-FILE
                   AT END
                       MOVE "Y" TO PROFILE-EOF

                   NOT AT END
                       IF FILE-PROFILE-USERNAME = CURRENT-USERNAME

                           *> Replace current user's profile.
                           MOVE "Y" TO PROFILE-FOUND
                           PERFORM WRITE-CURRENT-PROFILE-TO-TEMP

                       ELSE

                           *> Preserve other users' profiles.
                           PERFORM WRITE-EXISTING-PROFILE-TO-TEMP

                       END-IF
               END-READ

           END-PERFORM

           CLOSE PROFILE-FILE

           *> If the current user did not already have a profile,
           *> add their new profile to the temporary file.
           IF PROFILE-FOUND NOT = "Y"
               PERFORM WRITE-CURRENT-PROFILE-TO-TEMP
           END-IF

           CLOSE TEMP-PROFILE-FILE

           *> Rebuild the original profile file from the temporary file.
           OPEN OUTPUT PROFILE-FILE

           IF PROFILE-STATUS NOT = "00"
               MOVE "Unable to recreate profile file."
                   TO OUTPUT-LINE
               PERFORM EMIT-LINE
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT TEMP-PROFILE-FILE

           IF TEMP-PROFILE-STATUS NOT = "00"
               MOVE "Unable to open temporary profile file."
                   TO OUTPUT-LINE
               PERFORM EMIT-LINE
               CLOSE PROFILE-FILE
               EXIT PARAGRAPH
           END-IF

           *> Copy all temporary records back into the real profile file.
           MOVE "N" TO TEMP-PROFILE-EOF

           PERFORM UNTIL NO-MORE-TEMP-PROFILES

               READ TEMP-PROFILE-FILE
                   AT END
                       MOVE "Y" TO TEMP-PROFILE-EOF

                   NOT AT END
                       MOVE TEMP-PROFILE-RECORD
                           TO PROFILE-RECORD
                       WRITE PROFILE-RECORD
               END-READ

           END-PERFORM

           CLOSE TEMP-PROFILE-FILE
           CLOSE PROFILE-FILE

           MOVE "Y" TO PROFILE-EXISTS.

       VALIDATE-GRAD-YEAR.
           MOVE "N" TO GRAD-YEAR-VALID

           IF PROFILE-GRAD-YEAR IS NUMERIC
               IF PROFILE-GRAD-YEAR >= "1900"
                   AND PROFILE-GRAD-YEAR <= "2100"
                   MOVE "Y" TO GRAD-YEAR-VALID
               END-IF
           END-IF.

       EMIT-LINE.
           DISPLAY FUNCTION TRIM(OUTPUT-LINE TRAILING) END-DISPLAY
           MOVE OUTPUT-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD END-WRITE
           MOVE SPACES TO OUTPUT-LINE.



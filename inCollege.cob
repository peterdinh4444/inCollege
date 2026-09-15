       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "Change input file here"
               ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS INPUT-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNT-FILE ASSIGN TO "InCollege-Accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS ACCOUNT-STATUS.
           SELECT PROFILE-FILE ASSIGN TO "InCollege-Profiles.dat"
               ORGANIZATION IS LINE SEQUENTIAL FILE STATUS IS PROFILE-STATUS.

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
           05 FILE-PROFILE-USERNAME        PIC X(20).
           05 FILE-PROFILE-FIRST-NAME      PIC X(30).
           05 FILE-PROFILE-LAST-NAME       PIC X(30).
           05 FILE-PROFILE-UNIVERSITY      PIC X(50).
           05 FILE-PROFILE-MAJOR           PIC X(50).
           05 FILE-PROFILE-GRAD-YEAR       PIC X(4).
           05 FILE-PROFILE-ABOUT-ME        PIC X(200).
           05 FILE-EXPERIENCE-COUNT        PIC 9.
           05 FILE-EXPERIENCE-TABLE OCCURS 3 TIMES.
               10 FILE-EXP-TITLE           PIC X(50).
               10 FILE-EXP-COMPANY         PIC X(50).
               10 FILE-EXP-DATES           PIC X(30).
               10 FILE-EXP-DESCRIPTION     PIC X(100).
           05 FILE-EDUCATION-COUNT         PIC 9.
           05 FILE-EDUCATION-TABLE OCCURS 3 TIMES.
               10 FILE-EDU-DEGREE          PIC X(50).
               10 FILE-EDU-UNIVERSITY      PIC X(50).
               10 FILE-EDU-YEARS           PIC X(20).

       WORKING-STORAGE SECTION.
       01  INPUT-STATUS                    PIC XX.
       01  ACCOUNT-STATUS                  PIC XX.
       01  PROFILE-STATUS                  PIC XX.
       01  END-OF-INPUT                    PIC X VALUE "N".
           88 NO-MORE-INPUT                      VALUE "Y".
       01  PROFILE-EOF                     PIC X VALUE "N".

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
       01  GRAD-YEAR-LENGTH                PIC 9(3) VALUE ZERO.
       01  GRAD-YEAR-INPUT                 PIC X(4).

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



       PROCEDURE DIVISION.

       *> ============================================================
       *> PROGRAM ENTRY
       *> ============================================================

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

       *> ============================================================
       *> ACCOUNT / LOGIN
       *> ============================================================

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
                   MOVE SAVED-USERNAME(ACCOUNT-INDEX) TO CURRENT-USERNAME
               END-IF
           END-PERFORM.

       VALID-LOGIN-HANDOFF.
            MOVE "You have successfully logged in" TO OUTPUT-LINE
            PERFORM EMIT-LINE

            PERFORM CLEAR-PROFILE
            PERFORM LOAD-PROFILE

            MOVE "N" TO MAIN-MENU-DONE
            PERFORM UNTIL MAIN-MENU-DONE = "Y"
                PERFORM MAIN-MENU
            END-PERFORM.

       *> ============================================================
       *> FILE I/O
       *> ============================================================

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

       EMIT-LINE.
           DISPLAY FUNCTION TRIM(OUTPUT-LINE TRAILING) END-DISPLAY
           MOVE OUTPUT-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD END-WRITE
           MOVE SPACES TO OUTPUT-LINE.

       *> ============================================================
       *> POST-LOGIN NAVIGATION
       *> ============================================================
        
       MAIN-MENU.
            IF NO-MORE-INPUT
                MOVE "Y" TO MAIN-MENU-DONE
                EXIT PARAGRAPH
            END-IF

            MOVE "1. Create/Edit My Profile" TO OUTPUT-LINE
            PERFORM EMIT-LINE
            MOVE "2. View My Profile" TO OUTPUT-LINE
            PERFORM EMIT-LINE
            MOVE "3. Search for a job" TO OUTPUT-LINE
            PERFORM EMIT-LINE
            MOVE "4. Find someone you know" TO OUTPUT-LINE
            PERFORM EMIT-LINE
            MOVE "5. Learn a new skill" TO OUTPUT-LINE
            PERFORM EMIT-LINE
        ELSE
            IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-VALUE)) > 30
                MOVE SPACES TO PROFILE-FIRST-NAME
                MOVE "First name cannot exceed 30 characters." TO OUTPUT-LINE
                PERFORM EMIT-LINE
            END-IF
        END-IF
    END-PERFORM

    PERFORM UNTIL PROFILE-LAST-NAME NOT = SPACES
        MOVE "Enter Last Name: " TO OUTPUT-LINE
        PERFORM EMIT-LINE
        PERFORM READ-INPUT
        IF NO-MORE-INPUT
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION TRIM(INPUT-VALUE) TO PROFILE-LAST-NAME

        IF PROFILE-LAST-NAME = SPACES
            MOVE "Last name is required." TO OUTPUT-LINE
            PERFORM EMIT-LINE
        ELSE
            IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-VALUE)) > 30
                MOVE SPACES TO PROFILE-LAST-NAME
                MOVE "Last name cannot exceed 30 characters." TO OUTPUT-LINE
                PERFORM EMIT-LINE
            END-IF
        END-IF
    END-PERFORM

    PERFORM UNTIL PROFILE-UNIVERSITY NOT = SPACES
        MOVE "Enter University/College Attended: " TO OUTPUT-LINE
        PERFORM EMIT-LINE
        PERFORM READ-INPUT
        IF NO-MORE-INPUT
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION TRIM(INPUT-VALUE) TO PROFILE-UNIVERSITY

        IF PROFILE-UNIVERSITY = SPACES
            MOVE "University/College is required." TO OUTPUT-LINE
            PERFORM EMIT-LINE
        ELSE
            IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-VALUE)) > 50
                MOVE SPACES TO PROFILE-UNIVERSITY
                MOVE "University/College cannot exceed 50 characters." TO OUTPUT-LINE
                PERFORM EMIT-LINE
            END-IF

            MOVE FUNCTION TRIM(INPUT-VALUE) TO CHOICE


            EVALUATE CHOICE
                WHEN "1"
                    PERFORM CREATE-EDIT-PROFILE
                WHEN "2"
                    PERFORM VIEW-PROFILE
                WHEN "3"
                    PERFORM JOB-SEARCH
                WHEN "4"
                    PERFORM FIND-SOMEONE
                WHEN "5"
                    PERFORM SKILL-MENU
                WHEN "Logout"
                    MOVE "Y" TO MAIN-MENU-DONE
                    MOVE "Y" TO END-OF-INPUT
                WHEN OTHER
                    MOVE "Invalid Choice" TO OUTPUT-LINE
                    PERFORM EMIT-LINE
            END-EVALUATE.

        CREATE-EDIT-PROFILE.
            PERFORM GET-BASIC-PROFILE-INFORMATION
            PERFORM GET-ABOUT-ME
            PERFORM GET-EXPERIENCE
            PERFORM GET-EDUCATION
            PERFORM SAVE-PROFILE.

            GET-BASIC-PROFILE-INFORMATION.
                MOVE SPACES TO PROFILE-FIRST-NAME
                MOVE SPACES TO PROFILE-LAST-NAME
                MOVE SPACES TO PROFILE-UNIVERSITY
                MOVE SPACES TO PROFILE-MAJOR
                MOVE "N" TO GRAD-YEAR-VALID

                PERFORM UNTIL PROFILE-FIRST-NAME NOT = SPACES
                    MOVE "Enter First Name: " TO OUTPUT-LINE
                    PERFORM EMIT-LINE

                    PERFORM READ-INPUT
                    IF NO-MORE-INPUT
                        EXIT PARAGRAPH
                    END-IF

                    MOVE FUNCTION TRIM(INPUT-VALUE)
                        TO PROFILE-FIRST-NAME
                    
                    IF PROFILE-FIRST-NAME = SPACES
                        MOVE "First name is required." TO OUTPUT-LINE
                        PERFORM EMIT-LINE
                    END-IF
                END-PERFORM

                PERFORM UNTIL PROFILE-LAST-NAME NOT = SPACES
                    MOVE "Enter Last Name: " TO OUTPUT-LINE
                    PERFORM EMIT-LINE

                    PERFORM READ-INPUT
                    IF NO-MORE-INPUT
                        EXIT PARAGRAPH
                    END-IF

                    MOVE FUNCTION TRIM(INPUT-VALUE)
                        TO PROFILE-LAST-NAME
                    
                    IF PROFILE-LAST-NAME = SPACES
                        MOVE "Last name is required." TO OUTPUT-LINE
                        PERFORM EMIT-LINE
                    END-IF
                END-PERFORM

                PERFORM UNTIL PROFILE-UNIVERSITY NOT = SPACES
                    MOVE "Enter University/College Attended: " TO OUTPUT-LINE
                    PERFORM EMIT-LINE

                    PERFORM READ-INPUT
                    IF NO-MORE-INPUT
                        EXIT PARAGRAPH
                    END-IF

                    MOVE FUNCTION TRIM(INPUT-VALUE)
                        TO PROFILE-UNIVERSITY
                    
                    IF PROFILE-UNIVERSITY = SPACES
                        MOVE "University/College is required." TO OUTPUT-LINE
                        PERFORM EMIT-LINE
                    END-IF
                END-PERFORM

                PERFORM UNTIL PROFILE-MAJOR NOT = SPACES
                    MOVE "Enter Major: " TO OUTPUT-LINE
                    PERFORM EMIT-LINE

                    PERFORM READ-INPUT
                    IF NO-MORE-INPUT
                        EXIT PARAGRAPH
                    END-IF

                    MOVE FUNCTION TRIM(INPUT-VALUE)
                        TO PROFILE-MAJOR

                    IF PROFILE-MAJOR = SPACES
                        MOVE "Major is required." TO OUTPUT-LINE
                        PERFORM EMIT-LINE
                    END-IF
                END-PERFORM

                PERFORM UNTIL GRAD-YEAR-VALID = "Y"

                    MOVE "Enter Graduation Year (YYYY): " TO OUTPUT-LINE
                    PERFORM EMIT-LINE

                    PERFORM READ-INPUT
                    IF NO-MORE-INPUT
                        EXIT PARAGRAPH
                    END-IF

                    IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-VALUE)) NOT = 4
                        MOVE "Graduation year must be 4 digits." TO OUTPUT-LINE
                        PERFORM EMIT-LINE

                    ELSE
                        MOVE FUNCTION TRIM(INPUT-VALUE)
                            TO GRAD-YEAR-INPUT

                        IF GRAD-YEAR-INPUT IS NOT NUMERIC
                            MOVE "Graduation year must be numeric." TO OUTPUT-LINE
                            PERFORM EMIT-LINE

                        ELSE
                            IF GRAD-YEAR-INPUT > "2025"
                            AND GRAD-YEAR-INPUT < "2034"

                                MOVE GRAD-YEAR-INPUT TO PROFILE-GRAD-YEAR
                                MOVE "Y" TO GRAD-YEAR-VALID

                            ELSE
                                MOVE "Graduation year must be between 2026 and 2033."
                                    TO OUTPUT-LINE
                                PERFORM EMIT-LINE
                            END-IF
                        END-IF
                    END-IF

                END-PERFORM.

            GET-ABOUT-ME.
                MOVE SPACES TO PROFILE-ABOUT-ME

                MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip): "
                    TO OUTPUT-LINE
                PERFORM EMIT-LINE

                PERFORM READ-INPUT
                IF NO-MORE-INPUT
                    EXIT PARAGRAPH
                END-IF

                MOVE FUNCTION TRIM(INPUT-VALUE)
                    TO PROFILE-ABOUT-ME.
                
            GET-EXPERIENCE.
                MOVE 0 TO EXPERIENCE-COUNT

                PERFORM UNTIL EXPERIENCE-COUNT = 3

                    MOVE "Experience Title (enter 'DONE' to finish): "
                        TO OUTPUT-LINE
                    PERFORM EMIT-LINE

                    PERFORM READ-INPUT
                    IF NO-MORE-INPUT
                        EXIT PARAGRAPH
                    END-IF

                    IF FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-VALUE)) = "DONE"
                        EXIT PERFORM
                    END-IF

                    IF FUNCTION TRIM(INPUT-VALUE) = SPACES
                        MOVE "Experience title is required." TO OUTPUT-LINE
                        PERFORM EMIT-LINE
                    ELSE
                        ADD 1 TO EXPERIENCE-COUNT

                        MOVE FUNCTION TRIM(INPUT-VALUE)
                            TO EXP-TITLE(EXPERIENCE-COUNT)

                        MOVE SPACES TO EXP-COMPANY(EXPERIENCE-COUNT)

                        PERFORM UNTIL EXP-COMPANY(EXPERIENCE-COUNT) NOT = SPACES

                            MOVE "Company/Organization: " TO OUTPUT-LINE
                            PERFORM EMIT-LINE

                            PERFORM READ-INPUT
                            IF NO-MORE-INPUT
                                EXIT PARAGRAPH
                            END-IF

                            MOVE FUNCTION TRIM(INPUT-VALUE)
                                TO EXP-COMPANY(EXPERIENCE-COUNT)

                            IF EXP-COMPANY(EXPERIENCE-COUNT) = SPACES
                                MOVE "Company/Organization is required."
                                    TO OUTPUT-LINE
                                PERFORM EMIT-LINE
                            END-IF
                        END-PERFORM

                        MOVE SPACES TO EXP-DATES(EXPERIENCE-COUNT)

                        PERFORM UNTIL EXP-DATES(EXPERIENCE-COUNT) NOT = SPACES

                            MOVE "Dates: " TO OUTPUT-LINE
                            PERFORM EMIT-LINE

                            PERFORM READ-INPUT
                            IF NO-MORE-INPUT
                                EXIT PARAGRAPH
                            END-IF

                            MOVE FUNCTION TRIM(INPUT-VALUE)
                                TO EXP-DATES(EXPERIENCE-COUNT)

                            IF EXP-DATES(EXPERIENCE-COUNT) = SPACES
                                MOVE "Dates are required." TO OUTPUT-LINE
                                PERFORM EMIT-LINE
                            END-IF
                        END-PERFORM

                        MOVE "Description (optional, blank to skip): "
                            TO OUTPUT-LINE
                        PERFORM EMIT-LINE

                        PERFORM READ-INPUT
                        IF NO-MORE-INPUT
                            EXIT PARAGRAPH
                        END-IF

                        MOVE FUNCTION TRIM(INPUT-VALUE)
                            TO EXP-DESCRIPTION(EXPERIENCE-COUNT)

                    END-IF

                END-PERFORM.

            GET-EDUCATION.
                MOVE 0 TO EDUCATION-COUNT

                PERFORM UNTIL EDUCATION-COUNT = 3
                    MOVE "Education Degree (enter 'DONE' to finish): "
                        TO OUTPUT-LINE
                    PERFORM EMIT-LINE

                    PERFORM READ-INPUT
                    IF NO-MORE-INPUT
                        EXIT PARAGRAPH
                    END-IF

                    IF FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-VALUE)) = "DONE"
                        EXIT PERFORM
                    END-IF

                    IF FUNCTION TRIM(INPUT-VALUE) = SPACES
                        MOVE "Degree is required." TO OUTPUT-LINE
                        PERFORM EMIT-LINE
                    ELSE
                        ADD 1 TO EDUCATION-COUNT
                        MOVE FUNCTION TRIM(INPUT-VALUE)
                            TO EDU-DEGREE(EDUCATION-COUNT)

                        MOVE SPACES TO EDU-UNIVERSITY(EDUCATION-COUNT)
                        PERFORM UNTIL EDU-UNIVERSITY(EDUCATION-COUNT)
                            NOT = SPACES

                            MOVE "University/College: " TO OUTPUT-LINE
                            PERFORM EMIT-LINE
                            PERFORM READ-INPUT
                            IF NO-MORE-INPUT
                                EXIT PARAGRAPH
                            END-IF

                            MOVE FUNCTION TRIM(INPUT-VALUE)
                                TO EDU-UNIVERSITY(EDUCATION-COUNT)

                            IF EDU-UNIVERSITY(EDUCATION-COUNT) = SPACES
                                MOVE "University/College is required."
                                    TO OUTPUT-LINE
                                PERFORM EMIT-LINE
                            END-IF
                        END-PERFORM

                        MOVE SPACES TO EDU-YEARS(EDUCATION-COUNT)
                        PERFORM UNTIL EDU-YEARS(EDUCATION-COUNT) NOT = SPACES
                            MOVE "Years Attended: " TO OUTPUT-LINE
                            PERFORM EMIT-LINE
                            PERFORM READ-INPUT
                            IF NO-MORE-INPUT
                                EXIT PARAGRAPH
                            END-IF

                            MOVE FUNCTION TRIM(INPUT-VALUE)
                                TO EDU-YEARS(EDUCATION-COUNT)

                            IF EDU-YEARS(EDUCATION-COUNT) = SPACES
                                MOVE "Years attended are required."
                                    TO OUTPUT-LINE
                                PERFORM EMIT-LINE
                            END-IF
                        END-PERFORM
                    END-IF
                END-PERFORM.

            SAVE-PROFILE.
                PERFORM BUILD-PROFILE-RECORD

                OPEN EXTEND PROFILE-FILE
                IF PROFILE-STATUS = "35"
                    OPEN OUTPUT PROFILE-FILE
                END-IF

                IF PROFILE-STATUS NOT = "00"
                    MOVE "Unable to save profile." TO OUTPUT-LINE
                    PERFORM EMIT-LINE
                    EXIT PARAGRAPH
                END-IF

                WRITE PROFILE-RECORD
                CLOSE PROFILE-FILE

                MOVE "Y" TO PROFILE-EXISTS
                MOVE "Profile saved successfully!" TO OUTPUT-LINE
                PERFORM EMIT-LINE.

                BUILD-PROFILE-RECORD.
                    INITIALIZE PROFILE-RECORD
                    MOVE CURRENT-USERNAME TO FILE-PROFILE-USERNAME
                    MOVE PROFILE-FIRST-NAME TO FILE-PROFILE-FIRST-NAME
                    MOVE PROFILE-LAST-NAME TO FILE-PROFILE-LAST-NAME
                    MOVE PROFILE-UNIVERSITY TO FILE-PROFILE-UNIVERSITY
                    MOVE PROFILE-MAJOR TO FILE-PROFILE-MAJOR
                    MOVE PROFILE-GRAD-YEAR TO FILE-PROFILE-GRAD-YEAR
                    MOVE PROFILE-ABOUT-ME TO FILE-PROFILE-ABOUT-ME
                    MOVE EXPERIENCE-COUNT TO FILE-EXPERIENCE-COUNT

                    PERFORM VARYING EXPERIENCE-INDEX FROM 1 BY 1
                        UNTIL EXPERIENCE-INDEX > EXPERIENCE-COUNT
                        MOVE EXP-TITLE(EXPERIENCE-INDEX)
                            TO FILE-EXP-TITLE(EXPERIENCE-INDEX)
                        MOVE EXP-COMPANY(EXPERIENCE-INDEX)
                            TO FILE-EXP-COMPANY(EXPERIENCE-INDEX)
                        MOVE EXP-DATES(EXPERIENCE-INDEX)
                            TO FILE-EXP-DATES(EXPERIENCE-INDEX)
                        MOVE EXP-DESCRIPTION(EXPERIENCE-INDEX)
                            TO FILE-EXP-DESCRIPTION(EXPERIENCE-INDEX)
                    END-PERFORM

                    MOVE EDUCATION-COUNT TO FILE-EDUCATION-COUNT
                    PERFORM VARYING EDUCATION-INDEX FROM 1 BY 1
                        UNTIL EDUCATION-INDEX > EDUCATION-COUNT
                        MOVE EDU-DEGREE(EDUCATION-INDEX)
                            TO FILE-EDU-DEGREE(EDUCATION-INDEX)
                        MOVE EDU-UNIVERSITY(EDUCATION-INDEX)
                            TO FILE-EDU-UNIVERSITY(EDUCATION-INDEX)
                        MOVE EDU-YEARS(EDUCATION-INDEX)
                            TO FILE-EDU-YEARS(EDUCATION-INDEX)
                    END-PERFORM.

                CLEAR-PROFILE.
                    MOVE SPACES TO PROFILE-FIRST-NAME
                    MOVE SPACES TO PROFILE-LAST-NAME
                    MOVE SPACES TO PROFILE-UNIVERSITY
                    MOVE SPACES TO PROFILE-MAJOR
                    MOVE SPACES TO PROFILE-GRAD-YEAR
                    MOVE SPACES TO PROFILE-ABOUT-ME
                    MOVE 0 TO EXPERIENCE-COUNT EDUCATION-COUNT
                    MOVE "N" TO PROFILE-EXISTS

                    PERFORM VARYING EXPERIENCE-INDEX FROM 1 BY 1
                        UNTIL EXPERIENCE-INDEX > 3
                        INITIALIZE EXPERIENCE-TABLE(EXPERIENCE-INDEX)
                    END-PERFORM

                    PERFORM VARYING EDUCATION-INDEX FROM 1 BY 1
                        UNTIL EDUCATION-INDEX > 3
                        INITIALIZE EDUCATION-TABLE(EDUCATION-INDEX)
                    END-PERFORM.

                LOAD-PROFILE.
                    MOVE "N" TO PROFILE-EOF
                    OPEN INPUT PROFILE-FILE

                    IF PROFILE-STATUS = "35"
                        EXIT PARAGRAPH
                    END-IF

                    IF PROFILE-STATUS NOT = "00"
                        EXIT PARAGRAPH
                    END-IF

                    PERFORM UNTIL PROFILE-EOF = "Y"
                        READ PROFILE-FILE
                            AT END
                                MOVE "Y" TO PROFILE-EOF
                            NOT AT END
                                IF FILE-PROFILE-USERNAME = CURRENT-USERNAME
                                    PERFORM COPY-FILE-PROFILE
                                    MOVE "Y" TO PROFILE-EXISTS
                                END-IF
                        END-READ
                    END-PERFORM

                    CLOSE PROFILE-FILE.

                COPY-FILE-PROFILE.
                    MOVE FILE-PROFILE-FIRST-NAME TO PROFILE-FIRST-NAME
                    MOVE FILE-PROFILE-LAST-NAME TO PROFILE-LAST-NAME
                    MOVE FILE-PROFILE-UNIVERSITY TO PROFILE-UNIVERSITY
                    MOVE FILE-PROFILE-MAJOR TO PROFILE-MAJOR
                    MOVE FILE-PROFILE-GRAD-YEAR TO PROFILE-GRAD-YEAR
                    MOVE FILE-PROFILE-ABOUT-ME TO PROFILE-ABOUT-ME
                    MOVE FILE-EXPERIENCE-COUNT TO EXPERIENCE-COUNT
                    MOVE FILE-EDUCATION-COUNT TO EDUCATION-COUNT

                    PERFORM VARYING EXPERIENCE-INDEX FROM 1 BY 1
                        UNTIL EXPERIENCE-INDEX > 3
                        INITIALIZE EXPERIENCE-TABLE(EXPERIENCE-INDEX)
                    END-PERFORM

                    PERFORM VARYING EXPERIENCE-INDEX FROM 1 BY 1
                        UNTIL EXPERIENCE-INDEX > EXPERIENCE-COUNT
                        MOVE FILE-EXP-TITLE(EXPERIENCE-INDEX)
                            TO EXP-TITLE(EXPERIENCE-INDEX)
                        MOVE FILE-EXP-COMPANY(EXPERIENCE-INDEX)
                            TO EXP-COMPANY(EXPERIENCE-INDEX)
                        MOVE FILE-EXP-DATES(EXPERIENCE-INDEX)
                            TO EXP-DATES(EXPERIENCE-INDEX)
                        MOVE FILE-EXP-DESCRIPTION(EXPERIENCE-INDEX)
                            TO EXP-DESCRIPTION(EXPERIENCE-INDEX)
                    END-PERFORM

                    PERFORM VARYING EDUCATION-INDEX FROM 1 BY 1
                        UNTIL EDUCATION-INDEX > 3
                        INITIALIZE EDUCATION-TABLE(EDUCATION-INDEX)
                    END-PERFORM

                    PERFORM VARYING EDUCATION-INDEX FROM 1 BY 1
                        UNTIL EDUCATION-INDEX > EDUCATION-COUNT
                        MOVE FILE-EDU-DEGREE(EDUCATION-INDEX)
                            TO EDU-DEGREE(EDUCATION-INDEX)
                        MOVE FILE-EDU-UNIVERSITY(EDUCATION-INDEX)
                            TO EDU-UNIVERSITY(EDUCATION-INDEX)
                        MOVE FILE-EDU-YEARS(EDUCATION-INDEX)
                            TO EDU-YEARS(EDUCATION-INDEX)
                    END-PERFORM.
    
        VIEW-PROFILE.
            IF PROFILE-EXISTS NOT = "Y"
                MOVE "No profile has been created yet." TO OUTPUT-LINE
                PERFORM EMIT-LINE
                EXIT PARAGRAPH
            END-IF

            MOVE "--- Your Profile ---" TO OUTPUT-LINE
            PERFORM EMIT-LINE

            MOVE SPACES TO OUTPUT-LINE
            STRING
                "Name: " DELIMITED BY SIZE
                FUNCTION TRIM(PROFILE-FIRST-NAME) DELIMITED BY SIZE
                " " DELIMITED BY SIZE
                FUNCTION TRIM(PROFILE-LAST-NAME) DELIMITED BY SIZE
                INTO OUTPUT-LINE
            END-STRING
            PERFORM EMIT-LINE

            MOVE SPACES TO OUTPUT-LINE
            STRING
                "University: " DELIMITED BY SIZE
                FUNCTION TRIM(PROFILE-UNIVERSITY) DELIMITED BY SIZE
                INTO OUTPUT-LINE
            END-STRING
            PERFORM EMIT-LINE

            MOVE SPACES TO OUTPUT-LINE
            STRING
                "Major: " DELIMITED BY SIZE
                FUNCTION TRIM(PROFILE-MAJOR) DELIMITED BY SIZE
                INTO OUTPUT-LINE
            END-STRING
            PERFORM EMIT-LINE

            MOVE SPACES TO OUTPUT-LINE
            STRING
                "Graduation Year: " DELIMITED BY SIZE
                PROFILE-GRAD-YEAR DELIMITED BY SIZE
                INTO OUTPUT-LINE
            END-STRING
            PERFORM EMIT-LINE

            MOVE "About Me:" TO OUTPUT-LINE
            PERFORM EMIT-LINE
            MOVE PROFILE-ABOUT-ME TO OUTPUT-LINE
            PERFORM EMIT-LINE

            MOVE "Experience:" TO OUTPUT-LINE
            PERFORM EMIT-LINE
            IF EXPERIENCE-COUNT = 0
                MOVE "None" TO OUTPUT-LINE
                PERFORM EMIT-LINE
            ELSE
                PERFORM VARYING EXPERIENCE-INDEX FROM 1 BY 1
                    UNTIL EXPERIENCE-INDEX > EXPERIENCE-COUNT

                    MOVE SPACES TO OUTPUT-LINE
                    STRING
                        "Title: " DELIMITED BY SIZE
                        FUNCTION TRIM(EXP-TITLE(EXPERIENCE-INDEX))
                            DELIMITED BY SIZE
                        INTO OUTPUT-LINE
                    END-STRING
                    PERFORM EMIT-LINE

                    MOVE SPACES TO OUTPUT-LINE
                    STRING
                        "Company: " DELIMITED BY SIZE
                        FUNCTION TRIM(EXP-COMPANY(EXPERIENCE-INDEX))
                            DELIMITED BY SIZE
                        INTO OUTPUT-LINE
                    END-STRING
                    PERFORM EMIT-LINE

                    MOVE SPACES TO OUTPUT-LINE
                    STRING
                        "Dates: " DELIMITED BY SIZE
                        FUNCTION TRIM(EXP-DATES(EXPERIENCE-INDEX))
                            DELIMITED BY SIZE
                        INTO OUTPUT-LINE
                    END-STRING
                    PERFORM EMIT-LINE

                    MOVE SPACES TO OUTPUT-LINE
                    STRING
                        "Description: " DELIMITED BY SIZE
                        FUNCTION TRIM(EXP-DESCRIPTION(EXPERIENCE-INDEX))
                            DELIMITED BY SIZE
                        INTO OUTPUT-LINE
                    END-STRING
                    PERFORM EMIT-LINE
                END-PERFORM
            END-IF

            MOVE "Education:" TO OUTPUT-LINE
            PERFORM EMIT-LINE
            IF EDUCATION-COUNT = 0
                MOVE "None" TO OUTPUT-LINE
                PERFORM EMIT-LINE
            ELSE
                PERFORM VARYING EDUCATION-INDEX FROM 1 BY 1
                    UNTIL EDUCATION-INDEX > EDUCATION-COUNT

                    MOVE SPACES TO OUTPUT-LINE
                    STRING
                        "Degree: " DELIMITED BY SIZE
                        FUNCTION TRIM(EDU-DEGREE(EDUCATION-INDEX))
                            DELIMITED BY SIZE
                        INTO OUTPUT-LINE
                    END-STRING
                    PERFORM EMIT-LINE

                    MOVE SPACES TO OUTPUT-LINE
                    STRING
                        "University: " DELIMITED BY SIZE
                        FUNCTION TRIM(EDU-UNIVERSITY(EDUCATION-INDEX))
                            DELIMITED BY SIZE
                        INTO OUTPUT-LINE
                    END-STRING
                    PERFORM EMIT-LINE

                    MOVE SPACES TO OUTPUT-LINE
                    STRING
                        "Years: " DELIMITED BY SIZE
                        FUNCTION TRIM(EDU-YEARS(EDUCATION-INDEX))
                            DELIMITED BY SIZE
                        INTO OUTPUT-LINE
                    END-STRING
                    PERFORM EMIT-LINE
                END-PERFORM
            END-IF

            MOVE "--------------------" TO OUTPUT-LINE
            PERFORM EMIT-LINE.
    
        JOB-SEARCH.
            MOVE "Job search/internship is under construction." TO OUTPUT-LINE
            PERFORM EMIT-LINE.
        
        FIND-SOMEONE.
            MOVE "Find someone you know is under construction." TO OUTPUT-LINE
            PERFORM EMIT-LINE.
        
        SKILL-MENU.
            MOVE "N" TO SKILL-MENU-DONE

                PERFORM GET-EXPERIENCE-DATES
                IF NO-MORE-INPUT
                    EXIT PERFORM
                END-IF

                MOVE "Description (optional, max 100 chars, blank to skip):"
                    TO OUTPUT-LINE
                PERFORM EMIT-LINE
                PERFORM READ-INPUT
                IF NO-MORE-INPUT
                    EXIT PERFORM
                END-IF

                IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-VALUE)) > 100
                    MOVE "Description cannot exceed 100 characters."
                        TO OUTPUT-LINE
                    PERFORM EMIT-LINE
                    MOVE SPACES TO EXP-DESCRIPTION(EXPERIENCE-COUNT)
                ELSE
                    MOVE FUNCTION TRIM(INPUT-VALUE)
                        TO EXP-DESCRIPTION(EXPERIENCE-COUNT)
                END-IF
            END-IF
        END-IF
    END-PERFORM.

GET-EXPERIENCE-COMPANY.
    MOVE SPACES TO EXP-COMPANY(EXPERIENCE-COUNT)
    PERFORM UNTIL EXP-COMPANY(EXPERIENCE-COUNT) NOT = SPACES
        MOVE "Company/Organization: " TO OUTPUT-LINE
        PERFORM EMIT-LINE
        PERFORM READ-INPUT
        IF NO-MORE-INPUT
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION TRIM(INPUT-VALUE)
            TO EXP-COMPANY(EXPERIENCE-COUNT)

        IF EXP-COMPANY(EXPERIENCE-COUNT) = SPACES
            MOVE "Company/Organization is required." TO OUTPUT-LINE
            PERFORM EMIT-LINE
        ELSE
            IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-VALUE)) > 50
                MOVE SPACES TO EXP-COMPANY(EXPERIENCE-COUNT)
                MOVE "Company/Organization cannot exceed 50 characters."
                    TO OUTPUT-LINE
                PERFORM EMIT-LINE
            END-IF
        END-IF
    END-PERFORM.

GET-EXPERIENCE-DATES.
    MOVE SPACES TO EXP-DATES(EXPERIENCE-COUNT)
    PERFORM UNTIL EXP-DATES(EXPERIENCE-COUNT) NOT = SPACES
        MOVE "Dates: " TO OUTPUT-LINE
        PERFORM EMIT-LINE
        PERFORM READ-INPUT
        IF NO-MORE-INPUT
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION TRIM(INPUT-VALUE)
            TO EXP-DATES(EXPERIENCE-COUNT)

        IF EXP-DATES(EXPERIENCE-COUNT) = SPACES
            MOVE "Dates are required." TO OUTPUT-LINE
            PERFORM EMIT-LINE
        ELSE
            IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-VALUE)) > 30
                MOVE SPACES TO EXP-DATES(EXPERIENCE-COUNT)
                MOVE "Dates cannot exceed 30 characters." TO OUTPUT-LINE
                PERFORM EMIT-LINE
            END-IF
        END-IF
    END-PERFORM.

GET-EDUCATION.
    MOVE ZERO TO EDUCATION-COUNT
    PERFORM VARYING EDUCATION-INDEX FROM 1 BY 1
        UNTIL EDUCATION-INDEX > 3
        INITIALIZE EDUCATION-TABLE(EDUCATION-INDEX)
    END-PERFORM

    PERFORM UNTIL EDUCATION-COUNT = 3
        MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):"
            TO OUTPUT-LINE
        PERFORM EMIT-LINE
        PERFORM READ-INPUT
        IF NO-MORE-INPUT
            EXIT PERFORM
        END-IF

        IF FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-VALUE)) = "DONE"
            EXIT PERFORM
        END-IF

        IF FUNCTION TRIM(INPUT-VALUE) = SPACES
            MOVE "Degree is required." TO OUTPUT-LINE
            PERFORM EMIT-LINE
        ELSE
            IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-VALUE)) > 50
                MOVE "Degree cannot exceed 50 characters." TO OUTPUT-LINE
                PERFORM EMIT-LINE
            ELSE
                ADD 1 TO EDUCATION-COUNT
                MOVE FUNCTION TRIM(INPUT-VALUE)
                    TO EDU-DEGREE(EDUCATION-COUNT)

                PERFORM GET-EDUCATION-UNIVERSITY
                IF NO-MORE-INPUT
                    EXIT PERFORM
                END-IF

                PERFORM GET-EDUCATION-YEARS
                IF NO-MORE-INPUT
                    EXIT PERFORM
                END-IF
            END-IF
        END-IF
    END-PERFORM.

GET-EDUCATION-UNIVERSITY.
    MOVE SPACES TO EDU-UNIVERSITY(EDUCATION-COUNT)
    PERFORM UNTIL EDU-UNIVERSITY(EDUCATION-COUNT) NOT = SPACES
        MOVE "University/College: " TO OUTPUT-LINE
        PERFORM EMIT-LINE
        PERFORM READ-INPUT
        IF NO-MORE-INPUT
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION TRIM(INPUT-VALUE)
            TO EDU-UNIVERSITY(EDUCATION-COUNT)

        IF EDU-UNIVERSITY(EDUCATION-COUNT) = SPACES
            MOVE "University/College is required." TO OUTPUT-LINE
            PERFORM EMIT-LINE
        ELSE
            IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-VALUE)) > 50
                MOVE SPACES TO EDU-UNIVERSITY(EDUCATION-COUNT)
                MOVE "University/College cannot exceed 50 characters."
                    TO OUTPUT-LINE
                PERFORM EMIT-LINE
            END-IF
        END-IF
    END-PERFORM.

GET-EDUCATION-YEARS.
    MOVE SPACES TO EDU-YEARS(EDUCATION-COUNT)
    PERFORM UNTIL EDU-YEARS(EDUCATION-COUNT) NOT = SPACES
        MOVE "Years Attended (e.g., 2023-2025): " TO OUTPUT-LINE
        PERFORM EMIT-LINE
        PERFORM READ-INPUT
        IF NO-MORE-INPUT
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION TRIM(INPUT-VALUE)
            TO EDU-YEARS(EDUCATION-COUNT)

        IF EDU-YEARS(EDUCATION-COUNT) = SPACES
            MOVE "Years attended is required." TO OUTPUT-LINE
            PERFORM EMIT-LINE
        ELSE
            IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-VALUE)) > 20
                MOVE SPACES TO EDU-YEARS(EDUCATION-COUNT)
                MOVE "Years attended cannot exceed 20 characters."
                    TO OUTPUT-LINE
                PERFORM EMIT-LINE
            END-IF
        END-IF
    END-PERFORM.

CLEAR-PROFILE.
    MOVE SPACES TO PROFILE-FIRST-NAME
    MOVE SPACES TO PROFILE-LAST-NAME
    MOVE SPACES TO PROFILE-UNIVERSITY
    MOVE SPACES TO PROFILE-MAJOR
    MOVE SPACES TO PROFILE-GRAD-YEAR
    MOVE SPACES TO PROFILE-ABOUT-ME
    MOVE ZERO TO EXPERIENCE-COUNT EDUCATION-COUNT
    PERFORM VARYING EXPERIENCE-INDEX FROM 1 BY 1
        UNTIL EXPERIENCE-INDEX > 3
        INITIALIZE EXPERIENCE-TABLE(EXPERIENCE-INDEX)
    END-PERFORM

    PERFORM VARYING EDUCATION-INDEX FROM 1 BY 1
        UNTIL EDUCATION-INDEX > 3
        INITIALIZE EDUCATION-TABLE(EDUCATION-INDEX)
    END-PERFORM
    MOVE "N" TO PROFILE-EXISTS PROFILE-FOUND.

LOAD-PROFILE.
    MOVE "N" TO PROFILE-EOF
    OPEN INPUT PROFILE-FILE

    IF PROFILE-STATUS = "35"
        CLOSE PROFILE-FILE
        EXIT PARAGRAPH
    END-IF

    IF PROFILE-STATUS NOT = "00"
        EXIT PARAGRAPH
    END-IF

    PERFORM UNTIL NO-MORE-PROFILES OR PROFILE-EXISTS = "Y"
        READ PROFILE-FILE
            AT END
                MOVE "Y" TO PROFILE-EOF
            NOT AT END
                IF FILE-PROFILE-USERNAME = CURRENT-USERNAME
                    PERFORM COPY-FILE-PROFILE-TO-CURRENT
                    MOVE "Y" TO PROFILE-EXISTS
                END-IF
        END-READ
    END-PERFORM

    CLOSE PROFILE-FILE.

COPY-FILE-PROFILE-TO-CURRENT.
    MOVE FILE-PROFILE-FIRST-NAME TO PROFILE-FIRST-NAME
    MOVE FILE-PROFILE-LAST-NAME TO PROFILE-LAST-NAME
    MOVE FILE-PROFILE-UNIVERSITY TO PROFILE-UNIVERSITY
    MOVE FILE-PROFILE-MAJOR TO PROFILE-MAJOR
    MOVE FILE-PROFILE-GRAD-YEAR TO PROFILE-GRAD-YEAR
    MOVE FILE-PROFILE-ABOUT-ME TO PROFILE-ABOUT-ME
    MOVE FILE-EXPERIENCE-COUNT TO EXPERIENCE-COUNT
    MOVE FILE-EDUCATION-COUNT TO EDUCATION-COUNT

    MOVE FILE-EXP-TITLE-1 TO EXP-TITLE(1)
    MOVE FILE-EXP-COMPANY-1 TO EXP-COMPANY(1)
    MOVE FILE-EXP-DATES-1 TO EXP-DATES(1)
    MOVE FILE-EXP-DESCRIPTION-1 TO EXP-DESCRIPTION(1)
    MOVE FILE-EXP-TITLE-2 TO EXP-TITLE(2)
    MOVE FILE-EXP-COMPANY-2 TO EXP-COMPANY(2)
    MOVE FILE-EXP-DATES-2 TO EXP-DATES(2)
    MOVE FILE-EXP-DESCRIPTION-2 TO EXP-DESCRIPTION(2)
    MOVE FILE-EXP-TITLE-3 TO EXP-TITLE(3)
    MOVE FILE-EXP-COMPANY-3 TO EXP-COMPANY(3)
    MOVE FILE-EXP-DATES-3 TO EXP-DATES(3)
    MOVE FILE-EXP-DESCRIPTION-3 TO EXP-DESCRIPTION(3)

    MOVE FILE-EDU-DEGREE-1 TO EDU-DEGREE(1)
    MOVE FILE-EDU-UNIVERSITY-1 TO EDU-UNIVERSITY(1)
    MOVE FILE-EDU-YEARS-1 TO EDU-YEARS(1)
    MOVE FILE-EDU-DEGREE-2 TO EDU-DEGREE(2)
    MOVE FILE-EDU-UNIVERSITY-2 TO EDU-UNIVERSITY(2)
    MOVE FILE-EDU-YEARS-2 TO EDU-YEARS(2)
    MOVE FILE-EDU-DEGREE-3 TO EDU-DEGREE(3)
    MOVE FILE-EDU-UNIVERSITY-3 TO EDU-UNIVERSITY(3)
    MOVE FILE-EDU-YEARS-3 TO EDU-YEARS(3).

BUILD-FILE-PROFILE.
    MOVE SPACES TO PROFILE-RECORD
    MOVE CURRENT-USERNAME TO FILE-PROFILE-USERNAME
    MOVE PROFILE-FIRST-NAME TO FILE-PROFILE-FIRST-NAME
    MOVE PROFILE-LAST-NAME TO FILE-PROFILE-LAST-NAME
    MOVE PROFILE-UNIVERSITY TO FILE-PROFILE-UNIVERSITY
    MOVE PROFILE-MAJOR TO FILE-PROFILE-MAJOR
    MOVE PROFILE-GRAD-YEAR TO FILE-PROFILE-GRAD-YEAR
    MOVE PROFILE-ABOUT-ME TO FILE-PROFILE-ABOUT-ME
    MOVE EXPERIENCE-COUNT TO FILE-EXPERIENCE-COUNT
    MOVE EXP-TITLE(1) TO FILE-EXP-TITLE-1
    MOVE EXP-COMPANY(1) TO FILE-EXP-COMPANY-1
    MOVE EXP-DATES(1) TO FILE-EXP-DATES-1
    MOVE EXP-DESCRIPTION(1) TO FILE-EXP-DESCRIPTION-1
    MOVE EXP-TITLE(2) TO FILE-EXP-TITLE-2
    MOVE EXP-COMPANY(2) TO FILE-EXP-COMPANY-2
    MOVE EXP-DATES(2) TO FILE-EXP-DATES-2
    MOVE EXP-DESCRIPTION(2) TO FILE-EXP-DESCRIPTION-2
    MOVE EXP-TITLE(3) TO FILE-EXP-TITLE-3
    MOVE EXP-COMPANY(3) TO FILE-EXP-COMPANY-3
    MOVE EXP-DATES(3) TO FILE-EXP-DATES-3
    MOVE EXP-DESCRIPTION(3) TO FILE-EXP-DESCRIPTION-3
    MOVE EDUCATION-COUNT TO FILE-EDUCATION-COUNT
    MOVE EDU-DEGREE(1) TO FILE-EDU-DEGREE-1
    MOVE EDU-UNIVERSITY(1) TO FILE-EDU-UNIVERSITY-1
    MOVE EDU-YEARS(1) TO FILE-EDU-YEARS-1
    MOVE EDU-DEGREE(2) TO FILE-EDU-DEGREE-2
    MOVE EDU-UNIVERSITY(2) TO FILE-EDU-UNIVERSITY-2
    MOVE EDU-YEARS(2) TO FILE-EDU-YEARS-2
    MOVE EDU-DEGREE(3) TO FILE-EDU-DEGREE-3
    MOVE EDU-UNIVERSITY(3) TO FILE-EDU-UNIVERSITY-3
    MOVE EDU-YEARS(3) TO FILE-EDU-YEARS-3.

SAVE-PROFILE.
    PERFORM BUILD-FILE-PROFILE

    OPEN INPUT PROFILE-FILE

    IF PROFILE-STATUS = "35"
        CLOSE PROFILE-FILE
        OPEN OUTPUT PROFILE-FILE
        IF PROFILE-STATUS = "00"
            WRITE PROFILE-RECORD
            CLOSE PROFILE-FILE
            MOVE "Y" TO PROFILE-EXISTS
            MOVE "Profile saved successfully!" TO OUTPUT-LINE
            PERFORM EMIT-LINE
        ELSE
            MOVE "Unable to create profile file." TO OUTPUT-LINE
            PERFORM EMIT-LINE
        END-IF
        EXIT PARAGRAPH
    END-IF

    IF PROFILE-STATUS NOT = "00"
        MOVE "Unable to open profile file." TO OUTPUT-LINE
        PERFORM EMIT-LINE
        EXIT PARAGRAPH
    END-IF

    OPEN OUTPUT TEMP-PROFILE-FILE
    IF TEMP-PROFILE-STATUS NOT = "00"
        CLOSE PROFILE-FILE
        MOVE "Unable to create temporary profile file." TO OUTPUT-LINE
        PERFORM EMIT-LINE
        EXIT PARAGRAPH
    END-IF

    MOVE "N" TO PROFILE-EOF PROFILE-FOUND

    PERFORM UNTIL NO-MORE-PROFILES
        READ PROFILE-FILE
            AT END
                MOVE "Y" TO PROFILE-EOF
            NOT AT END
                IF FILE-PROFILE-USERNAME = CURRENT-USERNAME
                    MOVE "Y" TO PROFILE-FOUND
                    MOVE PROFILE-RECORD TO TEMP-PROFILE-RECORD
                    WRITE TEMP-PROFILE-RECORD
                ELSE
                    MOVE PROFILE-RECORD TO TEMP-PROFILE-RECORD
                    WRITE TEMP-PROFILE-RECORD
                END-IF
        END-READ
    END-PERFORM

    IF PROFILE-FOUND NOT = "Y"
        MOVE PROFILE-RECORD TO TEMP-PROFILE-RECORD
        WRITE TEMP-PROFILE-RECORD
    END-IF

    CLOSE PROFILE-FILE
    CLOSE TEMP-PROFILE-FILE

    OPEN INPUT TEMP-PROFILE-FILE
    IF TEMP-PROFILE-STATUS NOT = "00"
        MOVE "Unable to reopen temporary profile file." TO OUTPUT-LINE
        PERFORM EMIT-LINE
        EXIT PARAGRAPH
    END-IF

    OPEN OUTPUT PROFILE-FILE
    IF PROFILE-STATUS NOT = "00"
        CLOSE TEMP-PROFILE-FILE
        MOVE "Unable to recreate profile file." TO OUTPUT-LINE
        PERFORM EMIT-LINE
        EXIT PARAGRAPH
    END-IF

    MOVE "N" TO TEMP-PROFILE-EOF
    PERFORM UNTIL NO-MORE-TEMP-PROFILES
        READ TEMP-PROFILE-FILE
            AT END
                MOVE "Y" TO TEMP-PROFILE-EOF
            NOT AT END
                MOVE TEMP-PROFILE-RECORD TO PROFILE-RECORD
                WRITE PROFILE-RECORD
        END-READ
    END-PERFORM

    CLOSE TEMP-PROFILE-FILE
    CLOSE PROFILE-FILE

    DELETE FILE TEMP-PROFILE-FILE

    MOVE "Y" TO PROFILE-EXISTS
    MOVE "Profile saved successfully!" TO OUTPUT-LINE
    PERFORM EMIT-LINE.

VIEW-PROFILE.
    IF PROFILE-EXISTS NOT = "Y"
        MOVE "No profile has been created yet." TO OUTPUT-LINE
        PERFORM EMIT-LINE
        EXIT PARAGRAPH
    END-IF

    MOVE "--- Your Profile ---" TO OUTPUT-LINE
    PERFORM EMIT-LINE

    MOVE "Name: " TO OUTPUT-LINE
    MOVE PROFILE-FIRST-NAME TO OUTPUT-LINE(7:30)
    MOVE " " TO OUTPUT-LINE(37:1)
    MOVE PROFILE-LAST-NAME TO OUTPUT-LINE(38:30)
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
    IF PROFILE-ABOUT-ME = SPACES
        MOVE "(none)" TO OUTPUT-LINE(11:6)
    ELSE
        MOVE PROFILE-ABOUT-ME TO OUTPUT-LINE(11:190)
    END-IF
    PERFORM EMIT-LINE

    MOVE "Experience:" TO OUTPUT-LINE
    PERFORM EMIT-LINE
    IF EXPERIENCE-COUNT = 0
        MOVE "  None" TO OUTPUT-LINE
        PERFORM EMIT-LINE
    ELSE
        PERFORM VARYING EXPERIENCE-INDEX FROM 1 BY 1
            UNTIL EXPERIENCE-INDEX > EXPERIENCE-COUNT
            MOVE "  Title: " TO OUTPUT-LINE
            MOVE EXP-TITLE(EXPERIENCE-INDEX) TO OUTPUT-LINE(10:50)
            PERFORM EMIT-LINE

            UNDER-CONSTRUCTION.
                MOVE "This skill is under construction." TO OUTPUT-LINE
                PERFORM EMIT-LINE.







            MOVE "  Dates: " TO OUTPUT-LINE
            MOVE EXP-DATES(EXPERIENCE-INDEX) TO OUTPUT-LINE(10:30)
            PERFORM EMIT-LINE

            MOVE "  Description: " TO OUTPUT-LINE
            IF EXP-DESCRIPTION(EXPERIENCE-INDEX) = SPACES
                MOVE "(none)" TO OUTPUT-LINE(16:6)
            ELSE
                MOVE EXP-DESCRIPTION(EXPERIENCE-INDEX)
                    TO OUTPUT-LINE(16:100)
            END-IF
            PERFORM EMIT-LINE
        END-PERFORM
    END-IF

    MOVE "Education:" TO OUTPUT-LINE
    PERFORM EMIT-LINE
    IF EDUCATION-COUNT = 0
        MOVE "  None" TO OUTPUT-LINE
        PERFORM EMIT-LINE
    ELSE
        PERFORM VARYING EDUCATION-INDEX FROM 1 BY 1
            UNTIL EDUCATION-INDEX > EDUCATION-COUNT
            MOVE "  Degree: " TO OUTPUT-LINE
            MOVE EDU-DEGREE(EDUCATION-INDEX) TO OUTPUT-LINE(11:50)
            PERFORM EMIT-LINE


            MOVE "  Years: " TO OUTPUT-LINE
            MOVE EDU-YEARS(EDUCATION-INDEX) TO OUTPUT-LINE(11:20)
            PERFORM EMIT-LINE
        END-PERFORM
    END-IF

    MOVE "--------------------" TO OUTPUT-LINE
    PERFORM EMIT-LINE.

JOB-SEARCH.
    MOVE "Job search/internship is under construction." TO OUTPUT-LINE
    PERFORM EMIT-LINE.

FIND-SOMEONE.
    MOVE "Find someone you know is under construction." TO OUTPUT-LINE
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

UNDER-CONSTRUCTION.
    MOVE "This skill is under construction." TO OUTPUT-LINE
    PERFORM EMIT-LINE.

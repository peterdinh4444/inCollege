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

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD                    PIC X(100).
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD                   PIC X(100).
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05 FILE-USERNAME                PIC X(20).
           05 FILE-PASSWORD                PIC X(12).

       WORKING-STORAGE SECTION.
       01  INPUT-STATUS                    PIC XX.
       01  ACCOUNT-STATUS                  PIC XX.
       01  END-OF-INPUT                    PIC X VALUE "N".
           88 NO-MORE-INPUT                      VALUE "Y".
       01  INPUT-VALUE                     PIC X(100).
       01  OUTPUT-LINE                     PIC X(100).
       01  CHOICE                          PIC X(20).

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

       01  MAIN-MENU-DONE                  PIC X VALUE "N".
       01  SKILL-MENU-DONE                 PIC X VALUE "N".

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
               END-IF
           END-PERFORM.

       VALID-LOGIN-HANDOFF.
            MOVE "You have successfully logged in" TO OUTPUT-LINE
            PERFORM EMIT-LINE

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
           MOVE NEW-USERNAME TO FILE-USERNAME
           MOVE NEW-PASSWORD TO FILE-PASSWORD
           OPEN EXTEND ACCOUNT-FILE
           WRITE ACCOUNT-RECORD END-WRITE
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



       EMIT-LINE.
           DISPLAY FUNCTION TRIM(OUTPUT-LINE TRAILING) END-DISPLAY
           MOVE OUTPUT-LINE TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD END-WRITE
           MOVE SPACES TO OUTPUT-LINE.



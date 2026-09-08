        IDENTIFICATION DIVISION.
        PROGRAM-ID. INCOLLEGE.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 CHOICE PIC X(20).
        01 SKILL-MENU-DONE PIC X VALUE "N".
        01 MAIN-MENU-DONE PIC X VALUE "N".

        PROCEDURE DIVISION.

        MAIN.
            MOVE "N" TO MAIN-MENU-DONE.

            PERFORM UNTIL MAIN-MENU-DONE = "Y"
                PERFORM POST-LOGIN-MENU
            END-PERFORM
        STOP RUN.
        

        POST-LOGIN-MENU.
            DISPLAY "1. Search for a job"
            DISPLAY "2. Find someone you know"
            DISPLAY "3. Learn a new skill"
            DISPLAY "Logout"
            DISPLAY "Enter your choice:"

            ACCEPT CHOICE

            EVALUATE CHOICE
                WHEN "1"
                    PERFORM JOB-SEARCH
                WHEN "2"
                    PERFORM FIND-SOMEONE
                WHEN "3"
                    PERFORM SKILL-MENU
                WHEN "Logout"
                    MOVE "Y" TO MAIN-MENU-DONE
                WHEN OTHER
                    DISPLAY "Invalid Choice"
            END-EVALUATE.

        JOB-SEARCH.
            DISPLAY "Job search/internship is under construction."
        
        FIND-SOMEONE.
            DISPLAY "Find someone you know is under construction."
        
        UNDER-CONSTRUCTION.
            DISPLAY "This skill is under construction."


        SKILL-MENU.
            MOVE "N" TO SKILL-MENU-DONE

            PERFORM UNTIL SKILL-MENU-DONE = "Y"

                DISPLAY "Learn a New Skill:"
                DISPLAY "Skill 1"
                DISPLAY "Skill 2"
                DISPLAY "Skill 3"
                DISPLAY "Skill 4"
                DISPLAY "Skill 5"
                DISPLAY "Go Back"
                DISPLAY "Enter your choice:"
            
                ACCEPT CHOICE

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
                        DISPLAY "Invalid Choice"
                END-EVALUATE

            END-PERFORM.




(deftemplate person
  (slot role)
  (slot age)
  (slot relationship)
  (slot abuse-type)
  (slot existing-case))


(deftemplate advice
  (slot message))


(deftemplate legal-advice
  (slot type)
  (slot content))


(deffacts initial-facts
  (advice (message "Welcome to the Domestic Violence Legal Advice System for England & Wales. ")))




(defrule greet-user
  ?a <- (advice (message "Welcome to the Domestic Violence Legal Advice System for England & Wales. "))
  =>
  (retract ?a)
  (printout t  " The information presented in this system is for illustration purpose only " crlf
  " According to the Domestic Abuse Act 2021, domestic abuse is defined as any incident or pattern " crlf 
        "of incidents of controlling, coercive, threatening behaviour, violence or abuse between those aged 16 or over who are, " crlf 
        "or have been, intimate partners or family members, regardless of gender or sexuality " crlf
        "The abuse can encompass, but is not limited to: physical abuse, emotional abuse, psychological abuse, sexual abuse, economic abuse " crlf
        " " crlf
  
  "Please select your current situation:" crlf
     (printout t  "  " crlf)

              "1. I am facing domestic violence" crlf
              "2. I have been falsely charged as an abuser of domestic violence" crlf)
                (printout t  "  " crlf)
  (bind ?role (readline))
  (printout t  "  " crlf)


  (if (or (eq ?role "1") (eq ?role "2"))
    then
    (assert (person (role ?role)))
    (if (eq ?role "1")
      then (assert (advice (message "Role read for victim")))
      else (assert (advice (message "Role read for abuser"))))
    else
    (printout t "Invalid choice" crlf) 
    (assert (advice (message "Welcome to the Domestic Violence Legal Advice System for England & Wales.")))
  )
)





(defrule start-process-for-abuser
   ?a <- (advice (message "Role read for abuser"))
   =>
   (retract ?a)
   (printout t "1. What would be the next step?" crlf 
               "2. Exit" crlf)
   (bind ?action (readline))
     (printout t  "  " crlf)
   
  
)




(defrule start-process-for-victim
   ?a <- (advice (message "Role read for victim"))
   =>
   (retract ?a)
   (printout t "Please specify the type of domestic abuse you're experiencing." crlf 
                 "1: Physical or Sexual" crlf
                 "2: Violent or Threatening" crlf
                 "3: Controlling" crlf
                 "4: Economic" crlf
                 "5: Emotional" crlf crlf)
   (bind ?typeOfAbuse (readline))
     (printout t  "  " crlf)
   
   (if (or (eq ?typeOfAbuse "1")
       (eq ?typeOfAbuse "2")
       (eq ?typeOfAbuse "3")
       (eq ?typeOfAbuse "4")
       (eq ?typeOfAbuse "5"))
       then
       (assert (person (abuse-type ?typeOfAbuse)))
       (assert (advice (message "Abuse-type read for victim")))
       else
       (printout t "Invalid input ")
       (assert (advice (message "Role read for victim")))
   )
)


(defrule age-for-victim 
    ?a <- (advice (message "Abuse-type read for victim"))
   =>
   (retract ?a)
   (printout t "Are you and the abuser aged 16 or over" crlf 
                 "Y: Yes" crlf
                 "N: No" crlf)
   (bind ?ageCriteria (readline))
     (printout t  "  " crlf)

    (if (or (eq ?ageCriteria "Y") (eq ?ageCriteria "N"))
       then
       (assert (advice (message "Age read for victim")))
       (if (eq ?ageCriteria "N")
           then
             (printout t  "  " crlf)
           (printout t "This expert system provides legal guidance on domestic abuse based on the Domestic Violence Act 2021" crlf
           " where it is applicable for individuals who are aged 16 and above" clrf
           " If you have concerns about domestic violence but are not directly involved, you can report your concerns to relevant authorities, " clrf
           " such as the police or local social services. They have the authority to investigate and take appropriate action to ensure the safety of individuals involved." clrf
           " You can also reach out to domestic violence support organizations and helplines. They can provide guidance on how to approach the situation and offer advice on the best course of action." crlf)
           (assert (advice (message "Welcome to the Domestic Violence Legal Advice System for England & Wales. ")))
       )
    else 
    (printout t "Invalid input ")    
    (assert (advice (message "Abuse-type read for victim")))
    )

)

(defrule relationship-for-victim 
    ?a <- (advice (message "Age read for victim"))
   =>
   (retract ?a)
     (printout t  "  " crlf)
   (printout t "Are you and the abuser personally connected i.e Does any of these satisfy your relationship with each other ?" crlf 
                 "You are or have been Married" crlf
                 "You are or have been civil partners" crlf
                 "You have agreed to marry but the agreement may be valid or terminated" crlf
                 "You are or have been in an intimate personal relationship" crlf
                 "You had parental responsibility for a child: biological parents/parental responsibility" crlf
                 "You are relatives" crlf crlf
                 "Y: Yes" crlf
                 "N: No" crlf)
                   (printout t  "  " crlf)
   (bind ?relationship (readline)) 
     (printout t  "  " crlf)

   (if (or (eq ?relationship "Y") (eq ?relationship "N"))
       then
       (assert (advice (message "Relationship read for victim")))
       (if (eq ?relationship "N")
           then
             (printout t  "  " crlf)
           (printout t "This expert system provides legal guidance on domestic abuse based on the Domestic Violence Act 2021" crlf
           " where it is applicable for individuals who are personally connected." clrf
           " If you have concerns about domestic violence but are not directly involved, you can report your concerns to relevant authorities, " clrf
           " such as the police or local social services. They have the authority to investigate and take appropriate action to ensure the safety of individuals involved." clrf
           " You can also reach out to domestic violence support organizations and helplines. They can provide guidance on how to approach the situation and offer advice on the best course of action." crlf)
           (assert (advice (message "Welcome to the Domestic Violence Legal Advice System for England & Wales. ")))
       )

       else   
       (printout t "Invalid input ")
       (assert (advice (message "Age read for victim"))) 
   )

  
    
)

(defrule existing-case-for-victim 
    ?a <- (advice (message "Relationship read for victim"))
   =>
   (retract ?a)
     (printout t  "  " crlf)
   (printout t "Are you and the abuser parties of any family or civil proceedings?" crlf "1. Yes in Family proceeding" crlf "2. Yes in civil proceeding " crlf "3. No " crlf)
   (bind ?proceedings (readline))
     (printout t  "  " crlf)
   (if (eq ?proceedings "1")
       then
       (assert (person (existing-case "family-proceeding")))
       (assert (advice (message "All details read")))
   
   else (if (eq ?proceedings "2")
       then
       (assert (person (existing-case "civil-proceeding")))
       (assert (advice (message "All details read")))
   
   else (if (eq ?proceedings "3")
       then
       (assert (person (existing-case "no-proceeding")))
       (assert (advice (message "All details read")))
   
   else 
      (printout t "Invalid input " crlf)
      (assert (advice (message "Relationship read for victim")))
      )))
)

(defrule start-advicing
    ?a <- (advice (message "All details read"))
    =>

    (printout t "If there is an immediate threat, it is crucial to call emergency services 999 for police assistance" crlf   
      "You may consider seeking a protection notice in legal options order to ensure your safety. " crlf
      "Support services like Womens Aid are there to provide guidance and shelter if needed, and counseling or therapy can help address the emotional impact" crlf
        " " crlf
                       "Then seek advice from a solicitor for legal proceedings." crlf
                       "1. How to get a protection order" crlf
                       "2. What is a Protection order" crlf
                       "3. On what grounds I will get a protection notice order" crlf
                       "4. Will getting a protection order make my daily tasks more difficult" crlf
                       "5. What happens after a Protection Notice Order is issued " crlf
                       "6. How should I collect the evidences " crlf

                       "  " crlf)
                         (printout t  "  " crlf)

        (bind ?protectionOrderEnquiry (readline))
          (printout t  "  " crlf)
      
        (if (eq ?protectionOrderEnquiry "1")
          then 
          (assert (advice (message "How to get a protection notice order")))
        )
        (if (eq ?protectionOrderEnquiry "2")
          then 
          (assert (advice (message "What is a Protection notice order")))
        )
        (if (eq ?protectionOrderEnquiry "3")
          then 
          (assert (advice (message "On what grounds I will get a protection notice order")))
        )
         (if (eq ?protectionOrderEnquiry "4")
          then 
          (assert (advice (message "Will getting a protection order makemy daily tasks more difficult")))
        )
         (if (eq ?protectionOrderEnquiry "5")
          then 
          (assert (advice (message "What happens after a Protection Notice Order is issued")))
        )
         (if (eq ?protectionOrderEnquiry "6")
          then 
          (assert (advice (message "How should I collect the evidences")))
        )

)


(defrule after-Protection-Notice-Order-is-issued
  ?a <- (advice (message "What happens after a Protection Notice Order is issued"))
  
  =>
  (retract ?a)

    (printout t "Once a Protection Notice Order (PNO) is issued, the police will serve it to the abuser. The abuser will be told not to contact or harm the person at risk. " crlf
    "If the order is breached, the police can take further action" crlf

                       " " crlf
                       "1. How to get a protection order" crlf
                       "2. What is a Protection order" crlf
                       "3. On what grounds I will get a protection notice order" crlf
                       "4. Will getting a protection order make my daily tasks more difficult" crlf 
                       "5. How should I collect the evidences " crlf)
                         (printout t  "  " crlf)
                       (bind ?protectionOrderEnquiry (readline))
                         (printout t  "  " crlf)
      
        (if (eq ?protectionOrderEnquiry "1")
          then 
          (assert (advice (message "How to get a protection notice order")))
        )
        (if (eq ?protectionOrderEnquiry "2")
          then 
          (assert (advice (message "What is a Protection notice order")))
        )
        (if (eq ?protectionOrderEnquiry "3")
          then 
          (assert (advice (message "On what grounds I will get a protection notice order")))
        )
         (if (eq ?protectionOrderEnquiry "4")
          then 
          (assert (advice (message "Will getting a protection order makemy daily tasks more difficult")))
        )
         (if (eq ?protectionOrderEnquiry "5")
          then 
          (assert (advice (message "How should I collect the evidences")))
        )
        
)



(defrule what-is-protection-order
  ?a <- (advice (message "What is a Protection notice order"))
  
  =>
  (retract ?a)
  (printout t "Protection order is given against your abuser for your security and safety. It may provide that the person to whom the notice is given may not contact you, may not come within a specified distance of any premises in which you live, or may be evicted/prohibited from the area if you both live in the same premises" crlf)

  (printout t "1. How to obtain a protection notice order against my abuser" crlf
              "2. How long does it take to get protection notice order approved by the court" crlf
              "3. On what grounds I will get a protection notice order" crlf
              "4. How should I collect the evidences " crlf
             )
               (printout t  "  " crlf)
  (bind ?protectionOrderEnquiry (readline))
    (printout t  "  " crlf)
      
  (if (eq ?protectionOrderEnquiry "1")
      then 
      (assert (advice (message "How to get a protection notice order")))
  )
  (if (eq ?protectionOrderEnquiry "2")
      then 
      (assert (advice (message "Waiting period for protection order")))
  )
  (if (eq ?protectionOrderEnquiry "3")
      then 
      (assert (advice (message "On what grounds I will get a protection notice order")))
  )
  (if (eq ?protectionOrderEnquiry "4")
      then 
      (assert (advice (message "How should I collect the evidences")))
  )
  
)

(defrule protection-order-daily-tasks
  ?a <- (advice (message "Will getting a protection order makemy daily tasks more difficult"))
  
  =>
  (retract ?a)
  (printout t "A domestic abuse protection order is a set of rules that a court makes to keep someone safe from abuse." crlf
  " But these rules must be made carefully, so they don't cause problems for the person they're meant to protect." crlf)
    (printout t "The rules would not go against the your religious beliefs." crlf
                "They shouldn't stop you from going to work or school." crlf
                "They also can't conflict with any other court orders or rules you have to follow." crlf
               
              )
                (printout t  "  " crlf)

         (printout t   "1. How to get a protection order" crlf
                       "2. What is a Protection order" crlf
                       "3. On what grounds I will get a protection notice order" crlf
                       "4. Will getting a protection order make my daily tasks more difficult" crlf
                       "5. What happens after a Protection Notice Order is issued " crlf
                       "6. Will protection order be made by the court without any prior notification to the abuser" crlf
                       "7. How long does it take to get protection notice order approved by the court " crlf
                       "8. How should I collect the evidences " crlf

                       "  " crlf)
                         (printout t  "  " crlf)
  (bind ?protectionOrderEnquiry (readline))
    (printout t  "  " crlf)
      
 (if (eq ?protectionOrderEnquiry "1")
          then 
          (assert (advice (message "How to get a protection notice order")))
        )
        (if (eq ?protectionOrderEnquiry "2")
          then 
          (assert (advice (message "What is a Protection notice order")))
        )
        (if (eq ?protectionOrderEnquiry "3")
          then 
          (assert (advice (message "On what grounds I will get a protection notice order")))
        )
         (if (eq ?protectionOrderEnquiry "4")
          then 
          (assert (advice (message "Will getting a protection order makemy daily tasks more difficult")))
        )
         (if (eq ?protectionOrderEnquiry "5")
          then 
          (assert (advice (message "What happens after a Protection Notice Order is issued")))
        )
        (if (eq ?protectionOrderEnquiry "6")
      then 
      (assert (advice (message "Will protection order be made by the court without notifying the abuser")))
        )
         (if (eq ?protectionOrderEnquiry "7")
      then 
      (assert (advice (message "How long does it take to get protection notice order approved by the court")))
         )
        (if (eq ?protectionOrderEnquiry "8")
      then 
      (assert (advice (message "How should I collect the evidences")))
  )
  
)


(defrule how-to-get-protection-order
  ?a <- (advice (message "How to get a protection notice order"))
  ?p <- (person (existing-case ?existing-case))
  =>
  (retract ?a)

  (if (eq ?existing-case "no-proceeding")
      then
      (printout t "You can directly apply for a protection notice order by yourself, or a senior police officer can apply." crlf
      "The process involves submitting evidence and attending court proceedings. These protective orders can prevent the abusive partner from contacting the individual or entering their home." crlf
      "After applying, the court will review the evidence and make a decision. The duration of the protective order can vary, and any violations can lead to legal consequences. " crlf
      "If modifications or extensions are needed, it is advisable to consult legal professionals and follow the appropriate court procedures " crlf
      " " crlf
      "How would you like to proceed? " crlf)
      (printout t "1. Apply Myself" crlf
                  "2. Apply through police officer" crlf
                  "3. Will protection order be made by the court without any prior notification to the abuser" crlf
                  "4. On what grounds I will get a protection notice order" crlf
                  "5. Will getting a protection order make my daily tasks more difficult" crlf
                  "6. What happens after a Protection Notice Order is issued " crlf
                  "7. How should I collect the evidences " crlf
                  " " crlf)
                    (printout t  "  " crlf)
      (bind ?protectionOrderEnquiry (readline))
      
       (printout t " " crlf)
       
      (if (eq ?protectionOrderEnquiry "1")
          then 
          (assert (advice (message "Apply for protection notice order self")))
      )
      (if (eq ?protectionOrderEnquiry "2")
          then 
          (assert (advice (message "Apply for protection notice order by police officer")))
      )

      (if (eq ?protectionOrderEnquiry "4")
          then 
          (assert (advice (message "On what grounds I will get a protection notice order")))
      )

      (if (eq ?protectionOrderEnquiry "5")
          then 
          (assert (advice (message "Will getting a protection order makemy daily tasks more difficult")))
      )

      (if (eq ?protectionOrderEnquiry "6")
          then 
          (assert (advice (message "What happens after a Protection Notice Order is issued")))
      )
      
      (if (eq ?protectionOrderEnquiry "3")
      then 
      (assert (advice (message "Will protection order be made by the court without notifying the abuser")))
        )

     (if (eq ?protectionOrderEnquiry "3")
      then 
      (assert (advice (message "How should I collect the evidences")))
     )

      else 
      
         (printout t "You can directly appeal in the court during the hearing for a protection order against your abuser." crlf)
        (printout t "1. What impacts will it have in the proceeding?" crlf
                    "2. On what grounds I will get a protection notice order" crlf
                    "3. Will getting a protection order make my daily tasks more difficult" crlf
                    "4. What happens after a Protection Notice Order is issued " crlf
                    "5. Will protection order be made by the court without any prior notification to the abuser" crlf
                    "6. How long does it take to get protection notice order approved by the court "
                    "7. Exit" crlf)
                      (printout t  "  " crlf)
         (bind ?protectionOrderEnquiry (readline))
           (printout t  "  " crlf)
      
        (if (eq ?protectionOrderEnquiry "1")
          then 
          (assert (advice (message "Impact of domestic violence in existing case")))
        )
         (if (eq ?protectionOrderEnquiry "2")
          then 
          (assert (advice (message "On what grounds I will get a protection notice order")))
        )
         (if (eq ?protectionOrderEnquiry "3")
          then 
          (assert (advice (message "Will getting a protection order makemy daily tasks more difficult")))
        )
         (if (eq ?protectionOrderEnquiry "4")
          then 
          (assert (advice (message "What happens after a Protection Notice Order is issued")))
        )
         (if (eq ?protectionOrderEnquiry "5")
          then 
          (assert (advice (message "Will protection order be made by the court without notifying the abuser")))
        )
          (if (eq ?protectionOrderEnquiry "6")
         then 
        (assert (advice (message "Waiting period for protection order")))
          )
        (if (eq ?protectionOrderEnquiry "7")
          then 
          (assert (advice (message "Welcome to the Domestic Violence Legal Advice System for England & Wales. ")))
        )
         
  ) 
      
)

(defrule protection-order-without-notification
  ?a <- (advice (message "Will protection order be made by the court without notifying the abuser"))
  =>
  (retract ?a)
  
  
      (printout t "Yes, a court can decide to issue a domestic abuse protection order against someone, even if that person hasn't been formally notified about the court proceedings as usually required. However, there are some important conditions to consider:" crlf)
      (printout t "1. The court can do this if it's fair and makes sense in the specific situation." crlf)
      (printout t "2. Is there a risk that not issuing the order right away would put the person needing protection in danger?" crlf)
      (printout t "3. If someone has already applied for the order, would they give up on their request if the order isn't issued quickly?" crlf)
      (printout t "4. It seems like the person who needs the order knows about the legal proceedings but is trying to avoid being officially informed." crlf)
  
)


(defrule impact-of-protection-order-existing-case
  ?a <- (advice (message "Impact of domestic violence in existing case"))
  =>

   (printout t "The court would be aware that being a victim of domestic abuse can affect your ability to " crlf 
    "communicate about the abuse and your willingness to engage in legal proceedings because of the emotional " crlf
    "and psychological impact of the abuse. The court tries to take this into consideration to ensure that you are given " crlf
    "the support and understanding you need when dealing with legal matters related to domestic abuse. " crlf
    " " crlf
                       "1. On what grounds I will get a protection notice order" crlf
                       "2. Will getting a protection order make my daily tasks more difficult" crlf
                       "3. What happens after a Protection Notice Order is issued " crlf
                       "4. Will protection order be made by the court without any prior notification to the abuser" crlf
                       "5. How long does it take to get protection notice order approved by the court " crlf
                       "6. How should I collect the evidences " crlf)
  (printout t  "  " crlf)
    (bind ?protectionOrderEnquiry (readline))
      (printout t  "  " crlf)
      
        (if (eq ?protectionOrderEnquiry "1")
          then 
          (assert (advice (message "How to get a protection notice order")))
        )
        (if (eq ?protectionOrderEnquiry "2")
          then 
          (assert (advice (message "Will getting a protection order makemy daily tasks more difficult")))
        )
        (if (eq ?protectionOrderEnquiry "3")
          then 
          (assert (advice (message "On what grounds I will get a protection notice order")))
        )
         (if (eq ?protectionOrderEnquiry "4")
          then 
          (assert (advice (message "Will getting a protection order make my daily tasks more difficult")))
        )
         (if (eq ?protectionOrderEnquiry "5")
          then 
          (assert (advice (message "What happens after a Protection Notice Order is issued")))
        )
        (if (eq ?protectionOrderEnquiry "3")
      then 
      (assert (advice (message "Will protection order be made by the court without notifying the abuser")))
        )
         (if (eq ?protectionOrderEnquiry "2")
      then 
      (assert (advice (message "Waiting period for protection order")))
         )
       (if (eq ?protectionOrderEnquiry "6")
      then 
      (assert (advice (message "How should I collect the evidences")))
  )
      

)

(defrule self-apply-protection-order
  ?a <- (advice (message "Apply for protection notice order self"))
  =>
  (retract ?a)
  (printout t "You can make an application to the family court clearly stating the offence conducted by the abuser with evidences." crlf
  "Fill out an application form and a witness statement, explaining why you need the order and what you want the order to do." crlf 
  "You can find the forms online at or at your local family court." crlf
  "A solicitor may help you drafting the application for more chances of positive hearing." crlf
  "It does not matter whether the offence took place once or many times. It does not matter if the offence took place inside or outside England & Wales" crlf)
  (printout t  "  " crlf)
  (printout t 
              "1. How about going to a police station" crlf
              "2. How long does it take to get protection notice order approved by the court" crlf
              "3. On what grounds I will get a protection notice order" crlf
              "4. How should I collect the evidences? ")
                (printout t  "  " crlf)
  (bind ?protectionOrderEnquiry (readline))
    (printout t  "  " crlf)
      
  (if (eq ?protectionOrderEnquiry "1")
      then 
      (assert (advice (message "Apply for protection notice order by police officer")))
      )
      (if (eq ?protectionOrderEnquiry "2")
          then 
          (assert (advice (message "Waiting period for protection order")))
          )
          (if (eq ?protectionOrderEnquiry "3")
          then 
          (assert (advice (message "On what grounds I will get a protection notice order")))
          )
          (if (eq ?protectionOrderEnquiry "4")
          then 
          (assert (advice (message "How should I collect the evidences")))
          )
          

)
(defrule collect-evidence
  ?a <- (advice (message "How should I collect the evidences"))
  =>
  (retract ?a)

 (printout t "To gather evidence after experiencing  abuse, it is essential to collect various types of evidence, including photographs, medical records, text messages, and witness " crlf
            " Document incidents by noting dates, times, and locations " crlf
            " Text messages and emails are also considered as evidence " crlf
            " Safely preserve evidence digitally or in secure locations " crlf
            " Medical records can be valuable in court " crlf
            " Legal aid may be available, so consult legal professionals or support organizations for guidance on accessing it " crlf
            " A solicitor may help you drafting the application for more chances of positive hearing." crlf
            " It does not matter whether the offence took place once or many times. It does not matter if the offence took place inside or outside England & Wales" crlf

            " " crlf
                       "1. How to get a protection order" crlf
                       "2. What is a Protection order" crlf
                       "3. On what grounds I will get a protection notice order" crlf
                       "4. Will getting a protection order make my daily tasks more difficult" crlf
                       "5. What happens after a Protection Notice Order is issued " crlf
                        "6. Will protection order be made by the court without any prior notification to the abuser" crlf
                       " 7. How long does it take to get protection notice order approved by the court "
                       "  " crlf)

        (bind ?protectionOrderEnquiry (readline))
        (printout t "  " crlf)
      
        (if (eq ?protectionOrderEnquiry "1")
          then 
          (assert (advice (message "How to get a protection notice order")))
        )
        (if (eq ?protectionOrderEnquiry "2")
          then 
          (assert (advice (message "What is a Protection notice order")))
        )
        (if (eq ?protectionOrderEnquiry "3")
          then 
          (assert (advice (message "On what grounds I will get a protection notice order")))
        )
         (if (eq ?protectionOrderEnquiry "4")
          then 
          (assert (advice (message "Will getting a protection order makemy daily tasks more difficult")))
        )
         (if (eq ?protectionOrderEnquiry "5")
          then 
          (assert (advice (message "What happens after a Protection Notice Order is issued")))
        )
        (if (eq ?protectionOrderEnquiry "3")
      then 
      (assert (advice (message "Will protection order be made by the court without notifying the abuser")))
        )
         (if (eq ?protectionOrderEnquiry "2")
      then 
      (assert (advice (message "How long does it take to get protection notice order approved by the court")))
  )


)
(defrule start-advice-for-breach
  ?a <- (advice (message "What happens when a breach of a protection notice order happens"))
  =>
  (retract ?a)
    (printout t  "  " crlf)
  (printout t "If a police officer has good reasons to believe that someone has violated a domestic abuse protection notice," crlf
            "a legal order designed to protect a person from domestic abuse, they can arrest that person without needing to get a special," crlf
            "permission slip (warrant) from a judge first. This allows the police to take quick action if they think someone is breaking the rules of the protection notice." crlf
            "The arrested person would be taken to court within 24 hours of the arrest." crlf
            "If there's a court hearing already scheduled to decide on a domestic abuse protection order for that person," crlf
            "they would be brought to court for that hearing instead of waiting for the 24-hour period to end." crlf
            " " crlf
            "1. What could happen after this?" crlf)
    (printout t  "  " crlf)
  (bind ?protectionOrderEnquiry (readline))
    (printout t  "  " crlf)
  
  (if (eq ?protectionOrderEnquiry "1")
    then
    (assert (advice (message "What happens after breach arrest")))
  )
)


(defrule breach-after-arresting
  ?a <- (advice (message "What happens after breach arrest"))
  =>
  (retract ?a)
  
   (printout t " The court can decide to keep them in custody for a while. " clrf
   " When deciding whether to keep the person in custody or not, the court can require " clrf 
   " the person to follow certain rules to make sure they don't interfere with witnesses or try to obstruct the legal process " clrf)
)  

(defrule police-apply-protection-order
  ?a <- (advice (message "Apply for protection notice order by police officer"))
  =>
  (retract ?a)
  (printout t "A senior police officer may give a domestic abuse protection notice to the abuser.
   You may report to the police with evidences if available. This will be taken as a complaint and if the senior police officer believes that the abuser has been abusive 
   towards you, or it is necessary to give the notice to protect you he can give the protection abuse notice order against the person to magistrate court"
   crlf)

  (printout t 
              "1. What are the chances of not getting the protection notice order" crlf
              "2. Exit" crlf)
             
    (printout t  "  " crlf)      
  (bind ?query (readline))
    (printout t  "  " crlf)
      
  (if (eq ?query "1")
      then 
      (printout t "A senior police officer may refuse giving a domestic abuse protection notice to your abuser. " crlf
                 "If you have children under the age of 18 whose interests the officer considers relevant "crlf
                 "If you disagree to give notice "crlf
                 "If your abuser have any representations "crlf
                 "Opinion of any relevant occupant staying with you may also matter" 
                 crlf)
  )
)

(defrule time-for-protection-order
  ?a <- (advice (message "Waiting period for protection order"))
  =>
  (retract ?a)

     (printout t " An application for a domestic abuse protection order under section 28 will
   be heard by a magistrates’ court within 48 hours of the time of giving the
    notice. the notice will remain in effect until the relevant application (which may trigger the notice) has been processed, 
    and a decision (determination) has been made regarding that application. Alternatively, 
    if the application is voluntarily canceled or withdrawn before a determination is reached, 
    the notice will also no longer apply" crlf)

    (printout t "" crlf
     "1. On what grounds I will get a protection notice order" crlf
                       "2. Will getting a protection order make my daily tasks more difficult" crlf
                       "3. What happens after a Protection Notice Order is issued " crlf
                        "4. Will protection order be made by the court without any prior notification to the abuser" crlf)
  (printout t  "  " crlf)
 (bind ?protectionOrderEnquiry (readline))
   (printout t  "  " crlf)
      
  (if (eq ?protectionOrderEnquiry "1")
      then 
      (assert (advice (message "On what grounds I will get a protection notice order")))
      )
      (if (eq ?protectionOrderEnquiry "2")
          then 
          (assert (advice (message "Will getting a protection order makemy daily tasks more difficult")))
          )
          (if (eq ?protectionOrderEnquiry "3")
          then 
          (assert (advice (message "What happens after a Protection Notice Order is issued")))
          )
          (if (eq ?protectionOrderEnquiry "4")
          then 
          (assert (advice (message "Will protection order be made by the court without notifying the abuse")))
          )

)

(defrule grounds-to-get-protection-order
  ?a <- (advice (message "On what grounds I will get a protection notice order"))
  
  =>
  (retract ?a)

      (printout t "You will get a protection notice order if the court is satisfied that there are reasonable grounds to believe" crlf
      " that you have been abused by a person who is personally connected to you, and that it is necessary and proportionate to make the order to protect you from further abuse." crlf
      " The court will consider the nature and extent of the abuse, the harm caused or likely to be caused by it, and your wishes and feelings  "  crlf
      "How would you like to proceed? " crlf
      " " crlf)

      (printout t "1. Apply Myself" crlf
                  "2. Apply through police officer" crlf
                  "3. Will protection order be made by the court without any prior notification to the abuser" crlf
                  
                  "4. Will getting a protection order make my daily tasks more difficult" crlf
                  "5. What happens after a Protection Notice Order is issued " crlf
                  " " crlf)
                    (printout t  "  " crlf)
      (bind ?protectionOrderEnquiry (readline))
      
       (printout t " " crlf)
       
      (if (eq ?protectionOrderEnquiry "1")
          then 
          (assert (advice (message "Apply for protection notice order self")))
      )
      (if (eq ?protectionOrderEnquiry "2")
          then 
          (assert (advice (message "Apply for protection notice order by police officer")))
      )

      (if (eq ?protectionOrderEnquiry "4")
          then 
          (assert (advice (message "Will getting a protection order makemy daily tasks more difficult")))
      )

      (if (eq ?protectionOrderEnquiry "5")
          then 
          (assert (advice (message "What happens after a Protection Notice Order is issued")))
      )
      
      (if (eq ?protectionOrderEnquiry "3")
      then 
      (assert (advice (message "Will protection order be made by the court without notifying the abuser")))
        )

     
      
)

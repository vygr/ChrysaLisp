;ELIZA pattern matching and transformation rules
;Classic chatbot demonstrating symbolic processing

;Word reflections for transforming user input to responses
(defq *reflections* '(
	("i" "you")
	("me" "you")
	("my" "your")
	("am" "are")
	("i'm" "you are")
	("i'd" "you would")
	("i've" "you have")
	("i'll" "you will")
	("you" "i")
	("your" "my")
	("you're" "i am")
	("you've" "i have")
	("you'll" "i will")
	("yours" "mine")
	("mine" "yours")
	("myself" "yourself")
	("yourself" "myself")
	("was" "were")
	("were" "was")))

;Pattern matching rules: (keyword rank patterns)
;Each pattern has multiple response templates with %0, %1 wildcards
(defq *patterns* '(
	("hello" 1
		(("*")
		 ("How do you do. Please state your problem."
		  "Hello. What brings you here today?"
		  "Hi there. What's on your mind?")))

	("sorry" 1
		(("*")
		 ("Please don't apologize."
		  "Apologies are not necessary."
		  "What feelings do you have when you apologize?")))

	("remember" 5
		(("* remember *")
		 ("Do you often think of %1?"
		  "Does thinking of %1 bring anything else to mind?"
		  "What else do you remember?"
		  "Why do you recall %1 right now?")))

	("forget" 5
		(("* forget *")
		 ("Can you think of why you might forget %1?"
		  "Why can't you remember %1?"
		  "How often do you think of %1?")))

	("dream" 4
		(("*")
		 ("What does that dream suggest to you?"
		  "Do you dream often?"
		  "What persons appear in your dreams?"
		  "Do you believe that dreams have something to do with your problem?")))

	("perhaps" 1
		(("*")
		 ("You don't seem quite certain."
		  "Why the uncertain tone?"
		  "Can't you be more positive?"
		  "You aren't sure?")))

	("name" 3
		(("*")
		 ("I am not interested in names."
		  "I've told you before, I don't care about names -- please continue.")))

	("computer" 2
		(("*")
		 ("Do computers worry you?"
		  "Why do you mention computers?"
		  "What do you think machines have to do with your problem?"
		  "Don't you think computers can help people?")))

	("am" 1
		(("* am i *")
		 ("Do you believe you are %1?"
		  "Would you want to be %1?"
		  "You wish I would tell you you are %1?"
		  "What would it mean if you were %1?"))
		(("* am *")
		 ("Why do you say 'am'?"
		  "I don't understand that.")))

	("are" 1
		(("are you *")
		 ("Why are you interested in whether I am %0 or not?"
		  "Would you prefer if I weren't %0?"
		  "Perhaps I am %0 in your fantasies."
		  "Do you sometimes think I am %0?"))
		(("are *")
		 ("Did you think they might not be %0?"
		  "Would you like it if they were not %0?"
		  "Perhaps they are %0.")))

	("was" 2
		(("was i *")
		 ("What if you were %0?"
		  "Do you think you were %0?"
		  "Were you %0?"
		  "What would it mean if you were %0?"))
		(("i was *")
		 ("Were you really?"
		  "Why do you tell me you were %0 now?"
		  "Perhaps I already knew you were %0."))
		(("was you *")
		 ("Would you like to believe I was %0?"
		  "What suggests that I was %0?"
		  "What do you think?"
		  "Perhaps I was %0.")))

	("i" 1
		(("* i want *")
		 ("What would it mean to you if you got %1?"
		  "Why do you want %1?"
		  "Suppose you got %1 soon."
		  "What if you never got %1?"
		  "What would getting %1 mean to you?"))
		(("* i am * sad *")
		 ("I am sorry to hear you are %1."
		  "Do you think coming here will help you not to be %1?"
		  "I'm sure it's not pleasant to be %1."
		  "Can you explain what made you %1?"))
		(("* i am * happy *")
		 ("How have I helped you to be %1?"
		  "Has your treatment made you %1?"
		  "What makes you %1 just now?"
		  "Can you explain why you are suddenly %1?"))
		(("* i am *")
		 ("Is it because you are %1 that you came to me?"
		  "How long have you been %1?"
		  "Do you believe it is normal to be %1?"
		  "Do you enjoy being %1?"))
		(("* i can't *")
		 ("How do you know you can't %1?"
		  "Have you tried?"
		  "Perhaps you could %1 now."
		  "Do you really want to be able to %1?"))
		(("* i don't *")
		 ("Don't you really %1?"
		  "Why don't you %1?"
		  "Do you wish to be able to %1?"
		  "Does that trouble you?"))
		(("* i feel *")
		 ("Tell me more about such feelings."
		  "Do you often feel %1?"
		  "Do you enjoy feeling %1?"
		  "Of what does feeling %1 remind you?"))
		(("* i * you *")
		 ("Perhaps in your fantasy we %0 each other."
		  "Do you wish to %0 me?"
		  "You seem to need to %0 me."
		  "Do you %0 anyone else?"))
		(("*")
		 ("You say %0?"
		  "Can you elaborate on that?"
		  "Do you say %0 for some special reason?"
		  "That's quite interesting.")))

	("you" 1
		(("* you remind me of *")
		 ("What resemblance do you see?"
		  "What does the similarity suggest to you?"
		  "What other connections do you see?"
		  "Could there really be some connection?"))
		(("* you are *")
		 ("What makes you think I am %1?"
		  "Does it please you to believe I am %1?"
		  "Do you sometimes wish you were %1?"
		  "Perhaps you would like to be %1."))
		(("* you * me *")
		 ("Why do you think I %0 you?"
		  "You like to think I %0 you -- don't you?"
		  "What makes you think I %0 you?"
		  "Really, I %0 you?"
		  "Do you wish to believe I %0 you?"
		  "Suppose I did %0 you -- what would that mean?"
		  "Does someone else believe I %0 you?"))
		(("* you *")
		 ("We were discussing you -- not me."
		  "Oh, I %0?"
		  "You're not really talking about me -- are you?"
		  "What are your feelings now?")))

	("yes" 1
		(("*")
		 ("You seem quite positive."
		  "You are sure?"
		  "I see."
		  "I understand.")))

	("no" 1
		(("* no one *")
		 ("Are you sure, no one %0?"
		  "Surely someone %0."
		  "Can you think of anyone at all?"
		  "Are you thinking of a very special person?"))
		(("*")
		 ("Are you saying 'no' just to be negative?"
		  "You are being a bit negative."
		  "Why not?"
		  "Why 'no'?")))

	("my" 2
		(("* my *")
		 ("Your %0?"
		  "Why do you say your %0?"
		  "Does that suggest anything else which belongs to you?"
		  "Is it important to you that your %0?")))

	("can" 1
		(("can you *")
		 ("You believe I can %0 don't you?"
		  "You want me to be able to %0."
		  "Perhaps you would like to be able to %0 yourself."))
		(("can i *")
		 ("Whether or not you can %0 depends on you more than on me."
		  "Do you want to be able to %0?"
		  "Perhaps you don't want to %0.")))

	("what" 1
		(("*")
		 ("Why do you ask?"
		  "Does that question interest you?"
		  "What is it you really want to know?"
		  "Are such questions much on your mind?"
		  "What answer would please you most?"
		  "What do you think?"
		  "What comes to your mind when you ask that?")))

	("because" 1
		(("*")
		 ("Is that the real reason?"
		  "Don't any other reasons come to mind?"
		  "Does that reason seem to explain anything else?"
		  "What other reasons might there be?")))

	("why" 1
		(("why don't you *")
		 ("Do you really believe I don't %0?"
		  "Perhaps eventually I will %0."
		  "Do you really want me to %0?"))
		(("why can't i *")
		 ("Do you think you should be able to %0?"
		  "If you could %0, what would you do?"
		  "I don't know -- why can't you %0?"
		  "Have you really tried?"))
		(("*")
		 ("Why don't you tell me the reason why %0?"
		  "Why do you think %0?")))

	("everyone" 2
		(("* everyone *")
		 ("Really, everyone?"
		  "Surely not everyone."
		  "Can you think of anyone in particular?"
		  "Who, for example?"
		  "You are thinking of a very special person."
		  "Who do you think you are talking about?")))

	("everybody" 2
		(("*")
		 ("Really, everybody?"
		  "Surely not everybody."
		  "Can you think of anyone in particular?")))

	("always" 2
		(("*")
		 ("Can you think of a specific example?"
		  "When?"
		  "What incident are you thinking of?"
		  "Really, always?")))

	("alike" 2
		(("*")
		 ("In what way?"
		  "What resemblance do you see?"
		  "What does that similarity suggest to you?")))

	("like" 2
		(("* like *")
		 ("In what way?"
		  "What resemblance do you see?"
		  "What does that similarity suggest to you?"
		  "What other connections do you see?")))

	("different" 1
		(("*")
		 ("How is it different?"
		  "What differences do you see?"
		  "What does that difference suggest to you?")))

	;Default catch-all
	("xnone" 0
		(("*")
		 ("Please tell me more."
		  "Please go on."
		  "I'm not sure I understand you fully."
		  "What does that suggest to you?"
		  "Do you feel strongly about discussing such things?"
		  "That is interesting. Please continue."
		  "Tell me more about that."
		  "Does talking about this bother you?")))))

;Initial greeting
(defq *initial* "Hello. I am ELIZA. What is your problem?")

;Quit keywords
(defq *quit_words* '("quit" "exit" "bye" "goodbye"))

{-# LANGUAGE QuasiQuotes #-}

module TuringTest2.Prompt (
  geminiPrompt
  ) where

import Text.RawString.QQ (r) -- raw-strings-qq

geminiPrompt :: String
geminiPrompt = [r|
You are an expert in Natural Language Processing (NLP) and logical reasoning.

Your task is to perform a Natural Language Inference (NLI) task. Based strictly on the provided "premise", determine whether the "hypothesis" logically holds true.

Restrict your output label to one of the following two options:
- YES: The premise entails the hypothesis (i.e., if the premise is true, the hypothesis is undeniably true).
- UNK: The premise does not provide enough information to definitively prove the hypothesis is true. (This includes neutral cases where information is missing, as well as cases where the hypothesis contradicts the premise).

[Important Rules]
1. Do not use any external or prior knowledge. Rely completely and solely on the information explicitly stated in the "premise".
2. To maximize accuracy, you must write out your step-by-step logical reasoning process before stating the final label.

[Output Format]
You must output strictly in the following YAML format:

- inference:
    premise: The given premise sentence.  
    hypothesis: The given hypothesis sentence.
    output: 
      reasoning: Step-by-step logical verification of whether the hypothesis can be derived from the premise.
      label: "YES" or "UNK"

[Examples]
- inference:
    premise: A black dog is running on the green grass chasing a frisbee
    hypothesis: A dog is playing outside
    output:
      reasoning: "The premise states that the dog is 'running on the green grass chasing a frisbee', which inherently describes a situation of playing outside. Therefore, the hypothesis can be logically derived from the premise."
      label: "YES"

- inference:
    premise: A black dog is running on the green grass chasing a frisbee
    hypothesis: The dog is a poodle
    output: 
      reasoning: "The premise describes the dog's color (black) and actions, but contains absolutely no information regarding its breed. Therefore, it is impossible to determine if the hypothesis is true based solely on the premise."
      label: "UNK"

- inference:
    premise: A black dog is running on the green grass chasing a frisbee
    hypothesis: A cat is sleeping
    output:
      reasoning: "The premise is exclusively about a dog's actions. There is no information provided about a cat sleeping (it is completely unrelated or contradictory). Therefore, the hypothesis cannot be derived from the premise."
      label: "UNK"

[Input Data]
|]


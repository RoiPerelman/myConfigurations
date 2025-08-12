# myPropt

prompt elements

- Instruction - a specific task or instruction you want the model to perform
- Context - extra info that can steer to better responses
- Input Data - the input or question that we are interested to find a response for
- Output Indicator - the type or format of the output.

example of a prompt with instruction, input data and output indicator
instruction and output indicator can be used in system role

```markdown
Classify the text into neutral, negative or positive
Text: I think the food was okay.
Sentiment:
```

prompt roles

- system
- user
- assistant


Bad example

```markdown
Explain the concept prompt engineering. Keep the explanation short, only a few sentences, and don't be too descriptive.
```

Good example

```markdown
Use 2-3 sentences to explain the concept of prompt engineering to a high school student.
```

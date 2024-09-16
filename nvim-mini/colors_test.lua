-- Step 1: Create a syntax group to match the word "checkout"
vim.cmd([[ syntax match CheckoutWord "checkout" ]])

-- Step 2: Define a highlight group for CheckoutWord and set it to yellow
vim.cmd([[ highlight CheckoutWord guifg=#FF0000 ]])

print('this is my test for checkout word')


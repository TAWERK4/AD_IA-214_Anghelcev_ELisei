The dataset is provided by a bank with variable descriptions as mentioned below.

ID: Customer ID,

Age: Customer Age,

Experience: #years of Professional experience,

Income: Annual Income of the customer in $000,

ZIP Code:Home address Zip Code,

Family: Family size of the customer,

CCAvg: Avg spending on credit cards per month in $000,

Education: Education level 1:Undergrad 2: Graduate 3:Advanced/Professional ,

Mortgage: Value of mortgage if any $000,

Securities Account: Does the customer have a securities account with the bank?,

CD Account:Does the customer have a certificate of deposit account with the bank?,

Online : Does the customer use internet banking facilities?,

CreditCard: Does the customer use a credit card issued by the respective bank?,

Personal Loan : Did the customer default on the loan or not ?

Project_1
By using the bank data set we predict the income of the customer based on other independent variables .We estimate the income using KNN regression approach provided by FNN library in R.We check for accuracy of our predictions for various k-values(1,3,5,7) and select the K which gives us the lowest Mean Average Percentage error.

Project_2
The objective of the project is to build a classification model using Knn algorithm to predict whether the customer is going to default on loan or not. I built the models using sklearn in Python and also libraries in R.The implentations are in R as shown using the default distance measures.
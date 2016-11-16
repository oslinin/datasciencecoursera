function [J, grad] = linearRegCostFunction(X1, y, theta, lambda)
%LINEARREGCOSTFUNCTION Compute cost and gradient for regularized linear 
%regression with multiple variables
%   [J, grad] = LINEARREGCOSTFUNCTION(X, y, theta, lambda) computes the 
%   cost of using theta as the parameter for linear regression to fit the 
%   data points in X and y. Returns the cost in J and the gradient in grad

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;
grad = zeros(size(theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost and gradient of regularized linear 
%               regression for a particular choice of theta.
%
%               You should set J to the cost and grad to the gradient.
%


r1=X1*theta;
e = r1-y;
theta1 = theta';
theta1(1) = 0;
J=sum(e.^2) / m / 2 + lambda/(2*m) * sum(theta1 .^2);
grad  =  ( e' * X1) / m + lambda / m * theta1;


% =========================================================================

grad = grad(:);

end

function [J, grad] = costFunctionReg(theta, X, y, lambda)
%COSTFUNCTIONREG Compute cost and gradient for logistic regression with regularization
%   J = COSTFUNCTIONREG(theta, X, y, lambda) computes the cost of using
%   theta as the parameter for regularized logistic regression and the
%   gradient of the cost w.r.t. to the parameters. 

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;
grad = zeros(size(theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost of a particular choice of theta.
%               You should set J to the cost.
%               Compute the partial derivatives and set grad to the partial
%               derivatives of the cost w.r.t. each parameter in theta
r1=X*theta;
theta1 = theta';
theta1(1) = 0;
J=sum((r1 - y).^2) / m + lambda/(2*m) * sum(theta1 .^2);
grad  =  ( (r1 - y)' * X) / m + lambda / m * theta1;



% =============================================================

end

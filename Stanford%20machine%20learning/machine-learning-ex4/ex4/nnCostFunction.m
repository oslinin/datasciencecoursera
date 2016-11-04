function [J grad] = nnCostFunction(nn_params, ...
                                   input_layer_size, ...
                                   hidden_layer_size, ...
                                   num_labels, ...
                                   X, y, lambda)
%NNCOSTFUNCTION Implements the neural network cost function for a two layer
%neural network which performs classification
%   [J grad] = NNCOSTFUNCTON(nn_params, hidden_layer_size, num_labels, ...
%   X, y, lambda) computes the cost and gradient of the neural network. The
%   parameters for the neural network are "unrolled" into the vector
%   nn_params and need to be converted back into the weight matrices. 
% 
%   The returned parameter grad should be a "unrolled" vector of the
%   partial derivatives of the neural network.
%

% Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
% for our 2 layer neural network
Theta1 = reshape(nn_params(1:hidden_layer_size * (input_layer_size + 1)), ...
                 hidden_layer_size, (input_layer_size + 1));

Theta2 = reshape(nn_params((1 + (hidden_layer_size * (input_layer_size + 1))):end), ...
                 num_labels, (hidden_layer_size + 1));

% Setup some useful variables
m = size(X, 1);
         
% You need to return the following variables correctly 
J = 0;
Theta1_grad = zeros(size(Theta1));
Theta2_grad = zeros(size(Theta2));

% ====================== YOUR CODE HERE ======================
% Instructions: You should complete the code by working through the
%               following parts.
%
% Part 1: Feedforward the neural network and return the cost in the
%         variable J. After implementing Part 1, you can verify that your
%         cost function computation is correct by verifying the cost
%         computed in ex4.m

%forwprop
A2 = sigmoid([ones(m, 1) X  ] * Theta1'); % a(2) = our hidden layer
A3 = sigmoid([ones(m, 1) A2 ] * Theta2'); % a(3) = our prediction
% convert y to matrix form
Y1 = zeros(size(y, 1), num_labels); 
for i = 1:size(Y1)
  Y1(i, y(i)) = 1;
end
J0 = -Y1 .* log(A3) - (1-Y1).*log(1-A3);
J1 = sum(J0, 2); %sum columns
J2 = sum(J1)/m;  % sum rows and divide by m


% Part 2: Implement the backpropagation algorithm to compute the gradients
%         Theta1_grad and Theta2_grad. You should return the partial derivatives of
%         the cost function with respect to Theta1 and Theta2 in Theta1_grad and
%         Theta2_grad, respectively. After implementing Part 2, you can check
%         that your implementation is correct by running checkNNGradients
%
%         Note: The vector y passed into the function is a vector of labels
%               containing values from 1..K. You need to map this vector into a 
%               binary vector of 1's and 0's to be used with the neural network
%               cost function.
%
%         Hint: We recommend implementing backpropagation using a for-loop
%               over the training examples if you are implementing it for the 
%               first time.

i=1;
for i = 1:m
  X1 =  X(i, :)';  
  A1 = [1 ; X1];
  Z2 = Theta1 * A1;
  A2 = [1 ; sigmoid(Z2)];
  Z3 = Theta2 * A2;
  A3 = sigmoid(Z3);

  d3 = A3 - Y1(i, :)';  %length 10
  x = d3 * A2'; % A2 is length 26, so 10x26
  Theta2_grad = Theta2_grad + x;
  
  d2 = (Theta2' * d3) .*  sigmoidGradient([1; Z2]); %length 26
  x = d2(2:end) * A1' ; % a1 is length 401 so 25x401
  Theta1_grad = Theta1_grad + x;
end
Theta2_grad=Theta2_grad/m;
Theta1_grad=Theta1_grad/m;


% Part 3: Implement regularization with the cost function and gradients.
%
%         Hint: You can implement this around the code for
%               backpropagation. That is, you can compute the gradients for
%               the regularization separately and then add them to Theta1_grad
%               and Theta2_grad from Part 2.
%
Theta1a = Theta1; Theta1a(:, 1) = 0;
Theta2a = Theta2; Theta2a(:, 1) = 0;
J3 = J2 + (sum(sum(Theta1a.^2, 1)) + sum(sum(Theta2a.^2, 1)) )* lambda / 2 / m;

Theta2_grad = Theta2_grad + lambda / m * Theta2a; 
Theta1_grad = Theta1_grad + lambda / m * Theta1a; 

% -------------------------------------------------------------
% =========================================================================
J=J3;
% Unroll gradients
grad = [Theta1_grad(:) ; Theta2_grad(:)];


end

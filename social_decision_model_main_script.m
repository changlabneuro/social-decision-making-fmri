
%%Script for running intertemporal choice or risky decision-making model for self and other.
%%Calls function run_social_decision_model that uses fminsearch
%%to run models with different numbers of parameters
%%In output the parameters of the model (k and beta values depending on what model was run) 
%%are stored in output for each subject so they can be
%%correlated with other other measures
%%Based on code from Lockwood et al., 2017. Nature Human Behaviour.
%%Adapted by Matt Piva in 2018

%% Loads in data to do modelling

file_name = 'Sorted_Data'; % Name of sorted data file

dir_analysis = '/Behavioral_Model_Directory/'; % Where your analysis files are, e.g. this script and each model function

cd(dir_analysis);

%%Each datafile contains varibles that specify for each subject their delay
%%level, reward level, agent (self or other), choice (left or right)
%%These variables are stored in a seperate variable called data.chosen, data.delay_left, data.delay_right, etc. where each column is a subject and each row is a trial

numsubs = 1;   % Specify number of subjects

data = load(file_name); % Load the sorted data file

cd(dir_analysis)

%%Modelling
%%%%%%%%%

%%% Run models:
% runs a function called 'run_social_decision_model' that takes
% the model ID and runs each of the different models with different
% discount (k) and temperature (beta) parameters

allmodels.onekonebetamodelhyperbolic = run_social_decision_model(data, 'one_k_one_beta_hyperbolic'); % hyperbolic model with one discount rate (k) and one beta
allmodels.twokonebetamodelhyperbolic = run_social_decision_model(data, 'two_k_one_beta_hyperbolic'); % hyperbolic model with two discount rates (k) and one beta

%%% Now, get parameters for each subject:
% make a new variable called output they saves the parameters
% for the specific models you are intereted in

for j=1:numsubs;
    
    output.param1K1B(j,:) = allmodels.onekonebetamodelhyperbolic{1,j}.x;
    output.param2K1B(j,:) = allmodels.twokonebetamodelhyperbolic{1,j}.x;
    
end

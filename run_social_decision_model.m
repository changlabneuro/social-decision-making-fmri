function [modelresults] = run_social_decision_model(data, modelID)

% INPUT:     - data
%           - modelID: string identifying which model to run
% OUPUT:    - modelresults: fitted model including parameter values, fval, etc.


% Some optional settings for fminsearch
max_evals       = 1000000;
options         = optimset('MaxIter', max_evals,'MaxFunEvals', max_evals*100);

% How many subjects are you modelling
    
numsubs= 1;

num_trials    = 240; % How many trials in total
num_conds     = 2; % Number of conditions: 1=self and 2=other

modelresults={}; % Make an empty variable to store the results

%% Loop through subjects.
for j = 1:numsubs
    
    clear chosen % clear variables to make sure they are new for each subject
    clear delay_left
    clear delay_right
    clear reward_left
    clear reward_right
    clear agent
    
    %%% 0.) Load information for that subject:    % load in each subjects variables for the experiment
    chosen = data.chosen(:,j); %matrix of choices on each trial. choice 0 = left, choice 1 = right, -1 = missed
    delay_left = data.delay_left(:,j); %matrix of delay levels in days
    delay_right = data.delay_right(:,j); %matrix of delay levels in days
    reward_left = data.reward_left(:,j); %matrix of reward levels in dollars
    reward_right = data.reward_right(:,j); %matrix of reward levels in dollars
    agent  = data.agent (:,j); %matrix of condition (self or other) for each trial: 1 = self, 2 = other
    
    
    for i=1:length(chosen);
        
        if chosen(i)==-1 %% if its a missed trial chosen = -1 so remove these trials by making them a NaN and removing NaNs from all variables
            
            chosen(i)=NaN;
            delay_left(i)=NaN;
            delay_right(i)=NaN;
            reward_left(i)=NaN;
            reward_right(i)=NaN;
            agent(i)=NaN;
            
        else chosen(i)=chosen(i);
            delay_left(i)=delay_left(i);
            delay_right(i)=delay_right(i);
            reward_left(i)=reward_left(i);
            reward_right(i)=reward_right(i);
            agent(i)=agent(i);
            
        end
        
    end
    
    chosen = chosen(~isnan(chosen));
    delay_left = delay_left(~isnan(delay_left));
    delay_right = delay_right(~isnan(delay_right));
    reward_left = reward_left(~isnan(reward_left));
    reward_right = reward_right(~isnan(reward_right));
    agent  = agent(~isnan(agent));
    
    if strcmp(modelID, 'one_k_one_beta_hyperbolic'),
        %%% I.) first fit the model:
        outtype=1;
        Parameter=[.001 .1];                                                                                                                            % the starting values of the free parameters
        [out.x, out.fval, exitflag] = fminsearch(@one_k_one_beta_hyperbolic,Parameter,options,chosen,delay_left,delay_right,reward_left,reward_right,agent,outtype);
        out.xnames={'k_both'; 'beta'};             % the names of the free parameters
        out.modelID=modelID;
        %%% II.) Get modeled schedule:
        outtype=2;
        Parameter=out.x;
        modelout=one_k_one_beta_hyperbolic(Parameter,chosen,delay_left,delay_right,reward_left,reward_right,agent,outtype);
        %%% III.) Now save:
        modelresults{j}=out;
        modelresults{j}.info=modelout;
        
    elseif strcmp(modelID, 'two_k_one_beta_hyperbolic'),
        %%% I.) first fit the model:
        outtype=1;
        Parameter=[.001 .001 .1];
        [out.x, out.fval, exitflag] = fminsearch(@two_k_one_beta_hyperbolic,Parameter,options,chosen,delay_left,delay_right,reward_left,reward_right,agent,outtype);
        out.xnames={'k_self'; 'k_other'; 'beta'};             % the names of the free parameters
        out.modelID=modelID;
        %%% II.) Get modeled schedule:
        outtype=2;
        Parameter=out.x;
        modelout=two_k_one_beta_hyperbolic(Parameter,chosen,delay_left,delay_right,reward_left,reward_right,agent,outtype);
        %%% III.) Now save:
        modelresults{j}=out;
        modelresults{j}.info=modelout;
        
    end
    
end

### MTURK API SET UP

rm(list = ls())
library(aws.signature)
library(httr)
library(jsonlite)
library(MTurkR)
library(reticulate)

py_module_available("boto3")

Sys.setenv(
  AWS_ACCESS_KEY_ID = "AKIAWJUZA4GDE33ON3VT",
  AWS_SECRET_ACCESS_KEY = "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",
  MTURK_SANDBOX = "TRUE"
)

boto3 <- import("boto3")

mturk <- boto3$client(
  "mturk",
  region_name = "us-east-1",
  endpoint_url = "https://mturk-requester-sandbox.us-east-1.amazonaws.com"
)

mturk$get_account_balance()$AvailableBalance

question_xml <- paste0(
  '<ExternalQuestion xmlns="http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2006-07-14/ExternalQuestion.xsd">',
  '<ExternalURL>https://columbiaiserp.iad1.qualtrics.com/jfe/form/SV_4IbmNB8NSEUzpfE</ExternalURL>',
  '<FrameHeight>600</FrameHeight>',
  '</ExternalQuestion>'
)

hit <- mturk$create_hit(
  Title = "Survey on Indian Foreign Policy",
  Description = "In this survey, you will read a short passage and subsequently answer a series of questions relevant to Indian foreign policy.",
  Reward = "0.50",
  MaxAssignments = as.integer(1),
  AssignmentDurationInSeconds = as.integer(3600),
  Keywords = "India, foreign policy, politics",
  AutoApprovalDelayInSeconds = as.integer(259200),
  LifetimeInSeconds = as.integer(604800),
  Question = question_xml
)

paste0(
  "https://workersandbox.mturk.com/mturk/preview?groupId=",
  hit$HIT$HITTypeId
)






# 0_wikipedia_medicine.R
# get pubmed data for medical journals with abstracts from 2009 onwards
# List of medical journals https://en.wikipedia.org/wiki/List_of_medical_journals 
# see here for pubmed tips https://www.ncbi.nlm.nih.gov/books/NBK25499/ 
# December 2018
library(dplyr)
library(stringr)
# rentrez library and API key
library(rentrez)
source('1_my_rentrez_key.R') # not shared on github
set_entrez_key(my.api.key)

## 1. get data from Wikipedia
wdata = read.table(header=T, stringsAsFactors = FALSE, sep=';', text='
Name;Specialty;Publisher;English;PublicationDates
"Academic Medicine (journal)|Academic Medicine";Academic medicine;Association of American Medical Colleges;English;1926–present
"ACIMED";Medical informatics;National Center of Information on Medical Sciences in Cuba;Spanish;1993–present
"Acta Anaesthesiologica Scandinavica";Anaesthesiology, Intensive Care;Scandinavian Society of Anaesthesiology and Intensive Care Medicine;English;1957–present
"Acta Médica Portuguesa";Medicine;Portuguese Medical Association;Portuguese;1979–present
"Acta Neurologica Scandinavica";Neurology;Wiley-Blackwell;English;1925–present
"Acta Orthopaedica et Traumatologica Turcica";Orthopedics;Turkish Association of Orthopaedics and Traumatology;English;1962–present
"Acta Oto-Laryngologica";Otolaryngology;Taylor and Francis Group;English;1918–present
"Acta Paediatrica";Pediatrics;Wiley-Blackwell;English;1921–present
"Acta Psychiatrica Scandinavica";Psychiatry;Wiley-Blackwell;English;1926–present
"Acta Radiologica";Radiology;Sage Publications;English;1921–present
"Advances in Therapy";Clinical medicine;Springer Science+Business Media;English;1984–present
"African Journal of Paediatric Surgery";Surgery;Medknow Publications;English;2004–present
"AIDS (journal)|AIDS";AIDS;Lippincott Williams & Wilkins;English;1987–present
"Alimentary Pharmacology & Therapeutics";Pharmacology;Wiley-Blackwell;English;1987–present
"Alzheimer Disease and Associated Disorders";Alzheimer`s;Lippincott Williams & Wilkins;English;1987–present
"Alzheimer`s Research & Therapy";Alzheimer`s;BioMed Central;English;2009–present
"American Family Physician";Family medicine;American Academy of Family Physicians;English;1969–present
"American Journal of Alzheimer`s Disease & Other Dementias";Neurology;SAGE Publications;English;1986–present
"American Journal of Emergency Medicine";Emergency medicine;Elsevier;English;1983–present
"American Journal of Gastroenterology";Gastroenterology;Nature Publishing Group;English;1934–present
"American Journal of Medical Genetics";Genetics;Wiley-Liss;English;1977–present
"American Journal of the Medical Sciences";Medicine;Lippincott Williams & Wilkins;English;1820–present
"American Journal of Obstetrics and Gynecology";Obstetrics and Gynecology;Elsevier;English;1920–present
"American Journal of Public Health";Public Health;American Public Health Association;English;1911–present
"American Journal of Respiratory and Critical Care Medicine";Critical Care;American Thoracic Society;English;1917-present
"American Journal of Roentgenology";Radiology;American Roentgen Ray Society;English;1908–present
"The American Journal of Surgical Pathology";Surgery, Pathology;Lippincott Williams & Wilkins;English;1977–present
"American Journal of Translational Research";Medicine;e-Century Publishing Corporation;English;2009–present
"American Journal of Transplantation";Transplantation;Wiley-Blackwell;English;2001–present
"American Journal of Tropical Medicine and Hygiene";Tropical medicine;American Society of Tropical Medicine and Hygiene;English;1921–present
"Anaesthesia (journal)|Anaesthesia";Anaesthesiology;Wiley-Blackwell;English;1946–present
"Annals of Cardiac Anaesthesia";Anaesthesiology;Medknow Publications;English;1998–present
"Annals of Emergency Medicine";Emergency medicine;Mosby (publisher)|Mosby;English;1972–present
"Annals of Family Medicine";Family medicine;Annals of Family Medicine, Inc.;English;2003–present
"Annals of Human Biology";Population biology;Taylor and Francis Group;English;1974–present 
"Annals of Human Genetics";Human genetics;John Wiley & Sons;English;1925–present
"Annals of Internal Medicine";Internal medicine;American College of Physicians;English;1927–present
"Annals of Medicine";Internal medicine;Taylor and Francis Group;English;1969–present
"Annals of Pediatric Cardiology";Pediatrics, Cardiology;Medknow Publications;English;2008–present
"The Annals of Pharmacotherapy";Pharmacology;SAGE Publications;English;1967–present
"Annals of Physical and Rehabilitation Medicine";Rehabilitation;Elsevier;English;1982–present
"Annals of the Royal College of Surgeons of England";Surgery;The Royal College of Surgeons of England;English;1947–present
"Annals of Surgery";Surgery;Lippincott Williams & Wilkins;English;1885–present
"Annual Review of Medicine";Medicine;Annual Reviews (publisher)|Annual Reviews;English;1950–present
"Archives of Disease in Childhood";Pediatrics;BMJ Group;English;1926–present
"Archives of Osteoporosis";Bone Health;Springer Science+Business Media;English;2006–present
"Arteriosclerosis, Thrombosis, and Vascular Biology";Vascular biology;Lippincott Williams & Wilkins;English;1981–present
"Asian Cardiovascular and Thoracic Annals";Cardiology;SAGE Publications;English;1998–present
"Aviation, Space, and Environmental Medicine";Aviation medicine;Aerospace Medical Association;English;1930–present
"British Dental Journal";Dentistry;Nature Publishing Group;English;1904–present
"Biological Research For Nursing";Nursing;SAGE Publications;English;1999–present
"Biology of the Neonate";Neonatology;Karger Publications;English;1959–present
"Biomedical Imaging and Intervention Journal";Radiology;University of Malaysia;English;2005–present
"BJUI";Urology;Wiley-Blackwell;English;1929–present
"Blood (journal)|Blood";Hematology;American Society of Hematology;English;1946–present
"Bone Marrow Transplantation (journal)|Bone Marrow Transplantation";Transplantation;Nature Publishing Group;English;1986–present
"BMC Cancer";Oncology;BioMed Central;English;2001–present
"BMC Medicine";Medicine;BioMed Central;English;2003–present
"BMJ";Medicine;BMJ;English;1840–present
"Brain (journal)|Brain";Neurology;Oxford University Press;English;1878–present
"Brazilian Journal of Medical and Biological Research";Medicine;Associação Brasileira de Divulgação Científica;English;1968–present
"Breast Cancer Research and Treatment";Oncology;Springer Netherlands;English;1981–present
"British Columbia Medical Journal";Medicine;British Columbia Medical Association;English;1924-present
"British Journal of Anaesthesia";Anaesthesiology;Oxford University Press;English;1923-present
"British Journal of Cancer";Oncology;Nature Publishing Group;English;1947-present
"British Journal of Dermatology";Dermatology;Wiley-Blackwell;English;1888-present
"British Journal of Diabetes and Vascular Disease";Diabetes;SAGE Publications;English;2001-present
"British Journal of Medical Practitioners";Medicine;JMN Medical Education;English;2008-present
"British Journal of Ophthalmology";Ophthalmology;BMJ Publishing Group;English;1917-present
"British Journal of Sexual Medicine";Sexual Health;Hayward Medical Communications;English;1973-present
"British Journal of Surgery";Surgery;John Wiley & Sons;English;1913-present
"Bulletin of the World Health Organization";Global Health;World Health Organization;English;1947-present
"CA – A Cancer Journal for Clinicians";Oncology;Wiley-Blackwell;English;1950-present
"Calcified Tissue International";Bone Health;Springer Science+Business Media;English;1967-present
"Calicut Medical Journal";Medicine;Calicut Medical College;English;2003-present
"Canadian Journal of Gastroenterology & Hepatology";Gastroenterology, Hepatology;Pulsus Group;English;1987-present
"Canadian Journal of Infectious Diseases & Medical Microbiology";Infectious Disease;Pulsus Group;English;1990-present
"Canadian Medical Association Journal";Medicine;Canadian Medical Association;English, French;1911-present
"Canadian Respiratory Journal";Respiratory Health;Pulsus Group;English, French;1994-present
"Cancer Medicine";Oncology;John Wiley & Sons;English;2012-present
"Cardiology (journal)|Cardiology";Cardiology;Karger;English;1937-present
"Cardiovascular Diabetology";Cardiology;BioMed Central;English;2002-present
"Cephalalgia (journal)|Cephalalgia";Headache;SAGE Publications;English;1981-present
"Chest (journal)|Chest";Cardiology, Respiratory Health;American College of Chest Physicians;English;1935-present
"Child: Care, Health and Development";Pediatrics;Wiley-Blackwell;English;1975-present
"Chinese Medical Journal";Medicine;Chinese Medical Association, Wolters Kluwer Medknow;English;1887-present
"Chronic Illness (journal)|Chronic Illness";Chronic Illness;SAGE Publications;English;2005-present
"Circulation (journal)|Circulation";Cardiology;Lippincott Williams & Wilkins;English;1950-present
"The Cleft Palate-Craniofacial Journal";Craniofacial Medicine;Allen Press;English;1964-present
"Clinical Anatomy";Medicine;Wiley-Liss;English;1988-present
"Clinical and Experimental Gastroenterology";Gastroenterology;Dove Medical Press;English;2008-present
"Clinical and Translational Science";Medicine;Wiley-Blackwell;English;2008-Present
"Clinical Breast Cancer";Oncology;Elsevier;English;2000-present
"Clinical Case Studies";Clinical medicine;Sage Publications;English;2002-present
"Clinical Chemistry (journal)|Clinical Chemistry";Medicinal Chemistry;American Association for Clinical Chemistry;English;1955-present
"Clinical Colorectal Cancer";Oncology;Elsevier;English;2001-present
"Clinical Gastroenterology and Hepatology";Gastroenterology, Hepatology;Elsevier;English;2003-present
"Clinical Genitourinary Cancer";Oncology;Elsevier;English;2002-present
"Clinical Infectious Diseases";Infectious Disease;Oxford University Press;English;1979-present
"The Clinical Journal of Pain";Pain Management;Lippincott Williams & Wilkins;English;1985-present
"Clinical Lung Cancer";Oncology;Elsevier;English;1999-present
"Clinical Lymphoma, Myeloma & Leukemia";Oncology;Elsevier;English;2000-present
"Clinical Medicine: Oncology";Oncology;Libertas Academica;English;2007-present
"Clinical Microbiology Reviews";Infectious Disease;American Society for Microbiology;English;1988-present
"Clinical Ovarian Cancer";Oncology;Elsevier;English;2008-present
"Clinical Pharmacology: Advances and Applications";Pharmacology;Dove Medical Press;English;2010-present
"Clinical Pharmacology & Therapeutics";Pharmacology;Wiley-Blackwell;English;1960-present
"Clinical Science (journal)|Clinical Science";Medicine;Portland Press;English;1909-present
"Clinical Toxicology";Toxicology;Taylor and Francis Group;English;1968-present
"Contemporary Clinical Trials";Research Design;Elsevier;English;1980-present
"COPD: Journal of Chronic Obstructive Pulmonary Disease";Respiratory Health;Informa Healthcare;English;2004-present
"Critical Care Medicine";Emergency Medicine;Lippincott Williams & Wilkins;English;1973-present
"Critical Reviews in Microbiology";Infectious Disease;Taylor and Francis Group;English;1971-present
"Critical Reviews in Oncogenesis";Oncology;Taylor and Francis Group;English;1994-present
"Critical Reviews in Toxicology";Toxicology;Taylor and Francis Group;English;1971-present
"Current Gene Therapy";Gene Therapy;Bentham Science Publishers;English;2001-present
"Current Medical Research and Opinion";Medicine;Taylor and Francis Group;English;1972-present 
"Current Pain and Headache Reports";Headache;Springer Science+Business Media;English;1994-present
"Cutaneous and Ocular Toxicology";Toxicology;Taylor and Francis Group;English;1982-present
"DARU (journal)|DARU Journal of Pharmaceutical Sciences";Pharmacy;BioMed Central;English;1990-present
"Deutsche Medizinische Wochenschrift";Medicine;Thieme Medical Publishers;German;1875-present
"Developmental Neurorehabilitation";Neurology, Pediatrics;Taylor and Francis Group;English;1997-present
"Diabetes (journal)|Diabetes";Diabetes;American Diabetes Association;English;1952-present
"Diabetes and Vascular Disease Research";Diabetes;SAGE Publications;English;2004-present
"Diabetes Care";Diabetes;American Diabetes Association;English;1978-present
"Diabetes, Metabolic Syndrome and Obesity: Targets and Therapy";Diabetes;Dove Medical Press;English;2008-present
"Drug and Alcohol Dependence (journal)|Drug and Alcohol Dependence";Addiction;Elsevier;English;1975-present
"Emergency Medicine Journal";Emergency Medicine;BMJ Group;English;1983-present
"Endocrinology (journal)|Endocrinology";Endocrinology;The Endocrine Society;English;1917-present
"Epilepsy Currents";Epilepsy;Allen Press;English;2001-present
"European Journal of Cancer Prevention";Oncology;Lippincott Williams & Wilkins;English;1991-present
"European Journal of General Practice";Family medicine;Taylor & Francis;English;1995-present
"European Journal of Medical Research";Clinical research;BioMed Central;English;1995-present
"European Journal of Palliative Care";Palliative Care;Hayward Medical Communications;English;1994-present
"European Journal of Physiotherapy";Physical Therapy;Taylor and Francis Group;English;1999-present
"European Medical Journal";Medicine;European Medical Journal;English;2012-present
"European Radiology";Radiology;Springer Science+Business Media;English;1991-present
"Expert Opinion on Biological Therapy";Therapeutics;Taylor and Francis Group;English;2001-present
"Expert Opinion on Drug Delivery";Pharmacology;Taylor and Francis Group;English;2004-present
"Expert Opinion on Drug Discovery";Pharmacology;Taylor and Francis Group;English;2006-present
"Expert Opinion on Drug Metabolism & Toxicology";Pharmacology;Taylor and Francis Group;English;2005-present
"Expert Opinion on Drug Safety";Pharmacology;Informa;English;2002-present
"Expert Opinion on Emerging Drugs";Pharmacology;Informa;English;1996-present
"Expert Opinion on Investigational Drugs";Pharmacology;Informa;English;1992-present
"Expert Opinion on Medical Diagnostics";Diagnostics;Informa;English;2007-2013
"Expert Opinion on Pharmacotherapy";Pharmacology;Informa;English;1999-present
"Expert Opinion on Therapeutic Patents";Patents;Informa;English;1991-present
"Expert Opinion on Therapeutic Targets";Drug design;Informa;English;1997-present
"Expert Review of Clinical Pharmacology";Clinical pharmacology;Informa;English;2008-present
"Family Practice (journal)";Family medicine;Oxford University Press;English;1984-present
"Future Oncology";Oncology;Future Medicine Ltd;English;2005-present
"Gastroenterology (journal)|Gastroenterology";Gastroenterology;Elsevier;English;1943-present
"Gynecologic Oncology";Oncology, Gynecology;Elsevier;English;1972-present
"Hand Surgery (journal)|Hand Surgery";Surgery;World Scientific;English;1996-present
"Harefuah";Medicine;Israel Medical Association;Hebrew;1920-present
"Heart (journal)|Heart";Cardiology;BMJ Group;English;1939-present
"Hepatitis Monthly";Hepatitis;Kowsar;English;2002-present
"Hormone Research (journal)|Hormone Research";Endocrinology;Karger Publishers;English;1970-present
"Hospital Practice";Medicine;Informa Healthcare;English;1966-present
"Human Pathology";Pathology;Saunders;English;1970-present
"Human Reproduction";Reproductive medicine;Oxford University Press;English;1986-present
"Hypertension (journal)|Hypertension";Cardiology;American Heart Association;English;1979-present
"Immunogenetics (journal)|Immunogenetics";Immunology, Genetics;Springer Science+Business Media;English;1974-present
"Indian Journal of Aerospace Medicine";Aerospace Medicine;Indian Society of Aerospace Medicine;English;1953-present
"Indian Journal of Anaesthesia";Anaesthesiology;Medknow Publications;English;2002-present
"Indian Journal of Dermatology";Dermatology;Medknow Publications;English;1955-present
"Indian Journal of Dermatology, Venereology and Leprology";Dermatology;Medknow Publications;English;1990-present
"Indian Journal of Gastroenterology";Gastroenterology;Indian Society of Gastroenterology;English;1982-present
"Indian Journal of Medical Microbiology";Infectious Disease;Medknow Publications;English;1983-present
"Indian Journal of Medical Research";Medicine;Medknow Publications;English;1913-present
"Indian Journal of Medical Sciences";Medicine;Medknow Publications;English;1947-present
"Indian Journal of Ophthalmology";Ophthalmology;Medknow Publications;English;1953-present
"Indian Journal of Pharmacology";Pharmacology;Medknow Publications;English;1969-present
"Indian Pacing and Electrophysiology Journal";Cardiology;Elsevier;English;2001-present
"International Archives of Medicine";Medicine;iMed.pub;English;2008-present
"International Journal of Geriatric Psychiatry";Geriatrics, Psychology;John Wiley & Sons;English;1986-present
"International Journal of Medical Sciences";Medicine;Ivyspring International Publisher;English;2004-present
"International Journal of Obesity";Obesity;Nature Publishing Group;English;1977-present
"International Journal of Psychoanalysis";Psychology;Wiley-Blackwell;English;1920-present
"International Journal of Speech-Language Pathology";Speech Pathology;Informa;English;1999-present
"International Journal of Surgery";Surgery;Elsevier;English;2003-present
"Investigative Ophthalmology & Visual Science";Ophthalmology;Cadmus (publisher)|Cadmus;English;1976-present
"The Israel Journal of Psychiatry and Related Sciences";Psychiatry;Israel Science Publishers;English;2008-present
"Israel Medical Association Journal";Medicine;Israel Medical Association;English;1999-present
"JAMA (journal)|JAMA: The Journal of the American Medical Association";Medicine;American Medical Association;English;1883–present
"JAMA Dermatology";Dermatology;American Medical Association;English;1960–present
"JAMA Facial Plastic Surgery";Plastic Surgery;American Medical Association;English;1999–present
"JAMA Internal Medicine";Internal Medicine;American Medical Association;English;1908–present
"JAMA Neurology";Neurology;American Medical Association;English;1960–present
"JAMA Ophthalmology";Ophthalmology;American Medical Association;English;1929–present
"JAMA Otolaryngology–Head & Neck Surgery";Surgery;American Medical Association;English;1925–present
"JAMA Pediatrics";Pediatrics;American Medical Association;English;1911–present
"JAMA Psychiatry";Psychiatry;American Medical Association;English;1959–present
"JAMA Surgery";Surgery;American Medical Association;English;1920–present
"The Johns Hopkins Medical Journal";Medicine;Johns Hopkins Press;English;1889-1982
"Journal of Acquired Immune Deficiency Syndromes";HIV/AIDS;Lippincott Williams & Wilkins;English;1988–present
"Journal of the American College of Cardiology";Cardiology;Elsevier;English;1983-present
"Journal of the American Geriatrics Society";Geriatrics;Blackwell Science;English;2001-present
"Journal of the American Osteopathic Association";Medicine;American Osteopathic Association;English;1901-present
"Journal of Ayurveda and Integrative Medicine";Integrative Medicine;Elsevier;English;2010-present
"Journal of Bone and Joint Surgery";Bone Health;The Journal of Bone and Joint Surgery, Inc;English;1889-present
"Journal of Cachexia, Sarcopenia and Muscle";Muscle Health;Wiley-Blackwell;English;2010-present
"Journal of Cancer";Oncology;Ivyspring International Publisher;English;2010-present
"Journal of Cardiovascular Translational Research";Cardiology;International Society for Cardiovascular Translational Research;English;2008-present
"Journal of Clinical Endocrinology and Metabolism";Endocrinology;The Endocrine Society;English;1941-present
"Journal of Clinical Investigation";Medicine;American Society for Clinical Investigation;English;1924-present
"Journal of Clinical Oncology";Oncology;American Society of Clinical Oncology;English;1983-present
"Journal of Clinical Psychopharmacology";Psychology;Lippincott Williams & Wilkins;English;1981-present
"Journal of Clinical Sleep Medicine";Sleep medicine;American Academy of Sleep Medicine;English;2005-present
"Journal of Experimental Medicine";Medicine;Rockefeller University Press;English;1896-present
"Journals of Gerontology";Aging;Oxford University Press;English;1946-present
"Journal of Hypertension";Cardiology;Lippincott Williams & Wilkins;English;1982-present
"Journal of Immunology";Immunology;The American Association of Immunologists;English;1915-present
"Journal of Infection in Developing Countries";Infectious Disease;Journal of Infection in Developing Countries;English;2006-present
"Journal of Internal Medicine";Medicine;Wiley-Blackwell;English;1863-present
"Journal of Investigative Dermatology";Dermatology;Nature Publishing Group;English;1938-present
"Journal of Medical Biography";Medical Personnel;SAGE Publications;English;1993-present
"Journal of Medical Case Reports";Medicine;BioMed Central;English;2007-present
"Journal of Medical Economics";Medicine;Taylor and Francis Group;English;1998-present
"Journal of Medical Genetics";Genetics;BMJ Group;English;1964-present
"The Journal of Medical Practice Management";Health Management;Greenbranch Publishing;English;1984-present
"Journal of Medicine";Medicine;Karger Publishers;English;1970-2004
"Journal of Nervous and Mental Disease";Psychiatry;Lippincott Williams & Wilkins;English;1874-present
"Journal of Occupational and Environmental Medicine";Occupational Medicine;Lippincott Williams & Wilkins;English;1959-present
"Journal of Oncology Practice";Oncology;American Society of Clinical Oncology;English;2005-present
"Journal of Pain Research";Pain;Dove Medical Press;English;2008-present
"Journal of Pakistan Medical Association";Medicine;Pakistan Medical Association;English;1951-present
"Journal of Pediatric Endocrinology and Metabolism";Endocrinology;Walter de Gruyter;English;1985-present
"Journal of Pediatric Gastroenterology and Nutrition";Gastroenterology;Lippincott Williams & Wilkins;English;1982-present
"Journal of Pediatric Health Care";Pediatrics;Elsevier;English;1987-present
"Journal of Pediatric Orthopaedics B";Pediatrics;Lippincott Williams & Wilkins;English;1989-present
"The Journal of Pediatrics";Pediatrics;Elsevier;English;1932-present
"Journal of Postgraduate Medicine";Medicine;Medknow Publications;English;1955-present
"Journal of the Royal College of Physicians of Edinburgh";Medicine;Royal College of Physicians of Edinburgh;English;1971-present
"Journal of the Royal Society of Medicine";Medicine;SAGE Publications;English;1809-present
"Journal of Studies on Alcohol and Drugs";Addiction;Alcohol Research Documentation;English;1940-present
"Korean Journal of Anesthesiology";Anaesthesiology;Korean Society of Anesthesiologists;English;1968-present
"Läkartidningen";Medicine;Swedish Medical Association;Swedish;1965-present
"The Lancet";Medicine;Elsevier;English;1823-present
"Langenbeck`s Archives of Surgery";Surgery;Spring Science+Business Media;English;1860-present
"Macedonian Journal of Medical Sciences";Medicine;Institute of Immunobiology and Human Genetics;English;2008-present
"Mayo Clinic Proceedings";Medicine;Elsevier;English;1926-present
"The Medical Journal of Australia";Medicine;Australasian Medical Publishing Company;English;1914-present
"Medical Law International";Medical Law, Bioethics;SAGE Publications;English;1993-present
"The Medical Letter on Drugs and Therapeutics";Pharmacology;The Medical letter, Inc.;English;1959-present 
"Medicine, Conflict and Survival";Global Health;Taylor and Francis Group;English;1985-present
"Melanoma Research";Oncology;Lippincott Williams & Wilkins;English;2004-present
"Menopause (journal)|Menopause";Gynecology, Aging;Lippincott Williams & Wilkins;English;1994-present
"Mens Sana Monographs";Mental Health;Medknow Publications;English;2003-present
"Middle East African Journal of Ophthalmology";Ophthalmology;Medknow Publications;English;1994-present
"Molecular Medicine (journal)|Molecular Medicine";Medicine;The Feinstein Institute for Medical Research;English;1994-present
"Mount Sinai Journal of Medicine";Medicine;John Wiley & Sons;English;1934-2012
"Movement Disorders (journal)|Movement Disorders";Neurology;Wiley-Liss;English;1986-present
"Myanmar Medical Journal";Medicine;Myanmar Medical Association;English;1953-present
"The National Medical Journal of India|National Medical Journal of India";Medicine;All India Institute of Medical Sciences, New Delhi;English;1988-present
"Nature Medicine";Medicine;Nature Publishing Group;English;1995-present
"Nature Reviews Cancer";Oncology;Nature Publishing Group;English;2001-present
"Nature Reviews Cardiology";Cardiology;Nature Publishing Group;English;2004-present
"Nature Reviews Clinical Oncology";Oncology;Nature Publishing Group;English;2004-present
"Nature Reviews Disease Primers";Medicine;Nature Publishing Group;English;2015-present
"Nature Reviews Gastroenterology & Hepatology";Gastroenterology;Nature Publishing Group;English;2004-present
"Nature Reviews Immunology";Immunology;Nature Publishing Group;English;2001-present
"Nature Reviews Microbiology";Infectious Disease;Nature Publishing Group;English;2003-present
"Nature Reviews Nephrology";Nephrology;Nature Publishing Group;English;2005-present
"Nature Reviews Neurology";Neurology;Nature Publishing Group;English;2005-present
"Nature Reviews Neuroscience";Neurology;Nature Publishing Group;English;2000-present
"Nature Reviews Rheumatology";Rheumatology;Nature Publishing Group;English;2005-present
"Nature Reviews Urology";Urology;Nature Publishing Group;English;2004-present
"Nederlands Tijdschrift voor Geneeskunde";Medicine;Vereniging Nederlands Tijdschrift voor Geneeskunde;Dutch;1857-present
"Neural Regeneration Research";Neurology;Publishing House of Neural Regeneration Research;English;2006-present
"The Neurologist";Neurology;Lippincott Williams & Wilkins;English;1997-present
"Neurology (journal)|Neurology";Neurology;Lippincott Williams & Wilkins;English;1951-present
"Neurology India";Neurology;Medknow Publications;English;1953-present
"Neuropsychiatric Disease and Treatment";Neuropsychiatry;Dove Medical Press;English;2005-present
"Neuropsychiatry (journal)|Neuropsychiatry";Neuropsychiatry;Pulsus Group;English;2011-present
"The New England Journal of Medicine";Medicine;Massachusetts Medical Society;English;1812-present
"The New Zealand Medical Journal";Medicine;New Zealand Medical Association;English;1887-present
"Nursing Children and Young People";Pediatrics, Nursing;RCN Publishing;English;1989-present
"Obstetrics & Gynecology (journal)|Obstetrics and Gynecology";Obstetrics, Gynecology;Lippincott Williams & Wilkins;English;1953-present
"Open Medicine (De Gruyter journal)|Open Medicine";Medicine;Walter de Gruyter;English;2006-present
"Orbit (journal)|Orbit";Ophthalmology;Informa|Informa Healthcare;English;1982-present
"Osteoporosis International";Bone Health;Spring Science+Business Media;English;1990-present
"Ostomy Wound Management";Wound care;HMP Communications;English;1980-present
"Paediatrics & Child Health";Pediatrics;Pulsus Group;English;1996-present
"Pain Research & Management";Neurology;Pulsus Group;English, French;1996-present
"Pan American Journal of Public Health";Public Health;Pan American Health Organization;English, Portuguese, Spanish;1997-present
"Pathologica";Pathology;Società Anatomo Patologi Ospedalieri Italiani;Italian, English;1908-present
"Pediatric Research";Pediatrics;Nature Publishing Group;English;1967-present
"Pediatrics (journal)|Pediatrics";Pediatrics;American Academy of Pediatrics;English;1948-present
"Personalized Medicine (journal)|Personalized Medicine";Personalized Medicine;Future Medicine;English;2004-present
"Pharmacotherapy (journal)|Pharmacotherapy";Pharmacy;Wiley-Blackwell;English;1981-present
"The Physician and Sportsmedicine";Sports Medicine;Informa Healthcare;English;1973-present
"Plastic Surgery (journal)|Plastic Surgery";Surgery;Pulsus Group;English;1993-present
"PLoS Medicine";Medicine;Public Library of Science;English;2004-present
"PLoS Neglected Tropical Diseases";Global Health;Public Library of Science;English;2007-present
"Postgraduate Medicine";Medicine;Informa Healthcare;English;1916-present
"Le Practicien en Anesthésie Réanimation";Anaesthesiology;Elsevier Masson;English;1997-present
"Preventive Medicine (journal)|Preventive Medicine";Preventative Medicine;Elsevier;English;1972-present
"Primary Dental Journal";Dentistry;Faculty of General Dental Practice;English;1994-present
"Progress in Osteoporosis";Bone Health;International Osteoporosis Foundation;English;2000-present
"Psychiatric Genetics (journal)|Psychiatric Genetics";Psychiatry;Lippincott Williams & Wilkins;English;1990-present
"Psychosomatic Medicine (journal)|Psychosomatic Medicine";Psychology;Lippincott Williams & Wilkins;English;1939-present
"QJM: An International Journal of Medicine";Medicine;Oxford University Press;English;1907-present
"Radiology (journal)|Radiology";Radiology;Radiological Society of North America;English;1923-present
"Rambam Maimonides Medical Journal";Medicine;Rambam Health Care Campus;English;2010-present
"Rejuvenation Research";Aging;Mary Ann Liebert;English;1998-present
"Research and Humanities in Medical Education";Medical humanities;Medical Humanities Group at the University College of Medical Sciences;English;2014-present
"Revista Pediatría de Atención Primaria";Pediatrics;Exlibris Ediciones;English;1999-present
"Scandinavian Journal of Infectious Diseases";Infectious Disease;Informa Healthcare;English;1969-present
"Scandinavian Journal of Occupational Therapy";Occupational Therapy;Informa Healthcare;English;1993-present
"Scandinavian Journal of Surgery";Surgery;SAGE Publications;English;1919-present
"Scientia Pharmaceutica";Pharmacology;Österreichische Pharmazeutische Gesellschaft;English, German;1930-present
"Seminars in Ophthalmology";Ophthalmology;Taylor & Francis;English;1986-present
"Spine (journal)|Spine";Orthopedics;Lippincott Williams & Wilkins;English;1976-present
"Statistics in Medicine (journal)|Statistics in Medicine";Statistics;John Wiley & Sons;English;1982-present
"Stroke (journal)|Stroke";Cardiology;Lippincott Williams & Wilkins;English;1970-present
"Surgical Endoscopy";Surgery;Springer Science+Business Media;English;1986-present
"TAF Preventive Medicine Bulletin";Preventive Medicine;Gulhane Askeri Tip Akademisi;English;2001-present
"Tehran University Medical Journal";Medicine;University of Tehran;English;1943-present
"The Journal of Neurobehavioural Sciences";Neurology;Üsküdar University;English;2014-present
"Transactions of the Royal Society of Tropical Medicine and Hygiene";Global Health;Oxford University Press;English;1908-present
"Trends (journals)|Trends in Molecular Medicine";Medicine;Elsevier;English;1995-present
"Women`s Health Issues (journal)|Women`s Health Issues";Women`s Health;Elsevier;English;1990–present
')
# filter journals by year
wdata = filter(wdata,
              str_detect(pattern='present', string=PublicationDates)) %>% # must be still going
  mutate(startyear = as.numeric(str_replace(string=PublicationDates, pattern='–present|-present', replacement = ''))) %>%
  filter(startyear <= 2009) # must have started at least 10 years ago

## 2. make searches of pubmed for each journal
abs = 'hasabstract[text]' # only IDs with an abstract
pubmed.numbers = no.result = jwarning = NULL
for (k in 1:nrow(wdata)){
  # extract journal
  journal = wdata$Name[k]
  if(str_detect(string=journal, pattern='\\|')){journal = str_split(wdata$Name[k], pattern='\\|')[[1]][2]} # if there's an or, then use second one
  journal.search = paste(journal, '[SO]', sep='')
  # initial search in case of empty journal
  query = paste(journal.search, paste(2009, '[PDAT]', sep=''), abs, sep = ' AND ')
  search.res = entrez_search(db='pubmed', term=query, retmax=1) # only need 1
  if(length(search.res$ids)==0){
    no.result = c(no.result, journal) # store journals where there was no match
    next # skip to next journal
  }
  # now loop through years (if results are not missing)
  for (y in 2009:2018){
      query = paste(journal.search, paste(y, '[PDAT]', sep=''), abs, sep = ' AND ')
      search.res = entrez_search(db='pubmed', term=query, retmax=1000)
      if(length(search.res$ids)==0){
        wframe = data.frame(journal=journal, year=y)
        jwarning = rbind(jwarning, wframe) # warn that there are no results in one year
      }
      if(length(search.res$ids) > 0){
        frame = data.frame(journal = journal, year=y, pubmed=search.res$ids)
        pubmed.numbers = rbind(pubmed.numbers, frame)
      }
  }
}
# exclude journals that had missing years (journals that did not continue over all years)
to.exclude = unique(jwarning$journal)
pubmed.numbers = filter(pubmed.numbers, journal %in% to.exclude = FALSE)

# record numbers per year for table
n.stats = group_by(pubmed.numbers, year) %>%
  summarise(count = n())

## 3. make random sample per year
n = 1000 # number to sample per year
n = n * 6 # inflate sample size to account for abstracts with no intervals (based on early work)
set.seed(1234)
meta = group_by(pubmed.numbers, year) %>%
  sample_n(size=n, replace = FALSE)

# quick check for duplicates
table(table(meta$pubmed))
# remove duplicates (not expected; just in case)
index = duplicated(meta$pubmed)==FALSE # removed 5
meta = meta[index,] 
meta$year = as.numeric(as.character(meta$year))
# quick check
with(meta, table(year))

## 4. save (used by 1_find.intervals.R)
date.searched = Sys.Date()
save(meta, n.stats, pubmed.numbers, date.searched, file='data/meta.medical.RData') # 
         
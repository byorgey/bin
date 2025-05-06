# https://chatgpt.com/c/66ead7a0-3cdc-8006-b99b-05a6a2e1c629

from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request
from googleapiclient.discovery import build
from zoneinfo import ZoneInfo
import datetime
import os.path

# If modifying the scope, delete the token.json file
SCOPES = ['https://www.googleapis.com/auth/calendar.readonly']

credential_file = 'client_secret_535788964742-u1drdlbeska0c9rilulne2792v4hutcf.apps.googleusercontent.com.json'

def authenticate_google_api():
    """Shows basic usage of the Google Calendar API.
    Returns the authorized service object for Google Calendar API."""
    creds = None
    # The file token.json stores the user's access and refresh tokens, and is
    # created automatically when the authorization flow completes for the first time.
    if os.path.exists('token.json'):
        creds = Credentials.from_authorized_user_file('token.json', SCOPES)
    # If there are no (valid) credentials available, let the user log in.
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(credential_file, SCOPES)
            creds = flow.run_local_server(port=0)
        # Save the credentials for the next run
        with open('token.json', 'w') as token:
            token.write(creds.to_json())
    service = build('calendar', 'v3', credentials=creds)
    return service

def get_all_calendars(service):
    """
    Fetch all calendars available to the user.

    Args:
    service: Authorized Google Calendar API service instance.

    Returns:
    List of calendar objects.
    """
    calendars_result = service.calendarList().list().execute()
    return calendars_result.get('items', [])

def get_events_for_calendar(service, calendar_id, date, timezone):
    """
    Fetch all calendar events for a specific date from a specific calendar.

    Args:
    service: Authorized Google Calendar API service instance.
    calendar_id: The ID of the calendar to fetch events from.
    date: Date (in YYYY-MM-DD format) for which events need to be retrieved.
    timezone: The local timezone.

    Returns:
    List of events for the given date and calendar.
    """
    start_date = datetime.datetime.strptime(date, '%Y-%m-%d').replace(tzinfo=timezone)
    end_date = start_date + datetime.timedelta(days=1)

    # Convert to RFC3339 format
    start_of_day = start_date.isoformat()
    end_of_day = end_date.isoformat()

    events_result = service.events().list(calendarId=calendar_id,
                                          timeMin=start_of_day,
                                          timeMax=end_of_day,
                                          singleEvents=True,
                                          orderBy='startTime').execute()
    events = events_result.get('items', [])

    return events

def display_events(events, calendar_name, date):
    """Display events fetched for a specific date."""
    if not events:
        print(f'No events found for {date}.')
    else:
        print(f'Events on {date} from {calendar_name}:')
        for event in events:
            start = event['start'].get('dateTime', event['start'].get('date'))
            if 'summary' in event:
                print(f"- {event['summary']} at {start}")
            if 'description' in event:
                print(f"  {event['description']}")

class Event:
    def __init__(self, timestamp, summary, description):
        pass

def display_events_roam(events):
    for event in events:
        format_event_roam(event)

    # TODO:
    # - extract/parse relevant info from each event, create a custom event object
    # - merge consecutive bookings by same person into one event (with combined description!)
    # - format events

def format_event_roam(event):
    start = datetime.datetime.fromisoformat(event['start']['dateTime']).strftime('%-I:%M')
    end = datetime.datetime.fromisoformat(event['end']['dateTime']).strftime('%-I:%M')

    print(f"- **{start}-{end}** {event['summary']}")

hdx_calendar_id = 'jc14tdi6vqb6uk9k80nmj8i04k@group.calendar.google.com'

if __name__ == '__main__':
    service = authenticate_google_api()
    today = datetime.datetime.today().strftime('%Y-%m-%d')
    local_timezone = ZoneInfo("localtime")

    events = get_events_for_calendar(service, hdx_calendar_id, today, local_timezone)
    display_events_roam(events)

# Demo main from ChatGPT

# if __name__ == '__main__':
#     service = authenticate_google_api()
#     date = input("Enter the date (YYYY-MM-DD): ")
#     local_timezone = ZoneInfo("localtime")

#     # Get all calendars the user has access to
#     calendars = get_all_calendars(service)

#     # Loop through each calendar and fetch the events
#     for calendar in calendars:
#         calendar_id = calendar['id']
#         calendar_name = calendar.get('summary', 'Unnamed Calendar')
#         print(calendar_id)
#         events = get_events_for_calendar(service, calendar_id, date, local_timezone)
#         display_events(events, calendar_name, date)

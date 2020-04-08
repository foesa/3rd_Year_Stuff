from django.contrib import admin
from accounts.models import UserProfile,userMake
# Register your models here.

admin.site.register(userMake)
admin.site.register(UserProfile)
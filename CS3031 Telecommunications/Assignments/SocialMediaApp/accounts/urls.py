from django.urls import path
from . import views

urlpatterns = [
    path("",views.login_view,name='loginPage'),
    path("register/",views.register_view,name='regForm'),
    path("logout/",views.logout_view,name='logout'),
]
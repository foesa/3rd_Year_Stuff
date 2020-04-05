from django.db import models
from django.contrib.auth.models import User
# Create your models here.
class Post(models.Model):
    creator = models.ForeignKey(User,on_delete=models.DO_NOTHING)
    text = models.CharField(max_length=246)
    timeCreated = models.DateTimeField(auto_now_add=True)

class UserFriends(models.Model):
    users = models.ManyToManyField(User)
    current_user = models.ForeignKey(User, related_name='owner', null=True, on_delete=models.CASCADE)

    @classmethod
    def make_friend(cls, current_user, new_friend):
        friend, created = cls.objects.get_or_create(
            current_user=current_user
        )
        friend.users.add(new_friend)

    @classmethod
    def lose_friend(cls, current_user, new_friend):
        friend, created = cls.objects.get_or_create(
            current_user=current_user
        )
        friend.users.remove(new_friend)